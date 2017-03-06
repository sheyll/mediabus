module Data.MediaBus.Stream.Async
  ( withAsyncPolledSource
  , PayloadQ()
  , mkPayloadQ
  , payloadQSink
  , payloadQSource
  ) where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.State
import Control.Parallel.Strategies (NFData, rdeepseq, withStrategy)
import Data.Default
import Data.MediaBus.Basics.Clock
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream
import Data.MediaBus.Stream
import Data.Proxy
import Data.Time.Clock
import Debug.Trace
import System.Random
import Text.Printf

data PollPayloadSourceSt s t = MkPollPayloadSourceSt
  { _ppSeqNum :: !s
  , _ppTicks :: !t
  }

makeLenses ''PollPayloadSourceSt

withAsyncPolledSource
  :: ( MonadResource m
     , MonadBaseControl IO m
     , KnownRate r
     , Integral t
     , Integral s
     , Default p
     , HasStaticDuration c
     , HasDuration c
     , NFData c
     , NFData p
     , NFData s
     , NFData t
     , Random i
     , Random t
     , Random s
     , Show c
     )
  => Int
  -> Source m (Stream i s (Ticks r t) p c)
  -> ((Async (), Source m (Stream i s (Ticks r t) p (Discontinous c))) -> m o)
  -> m o
withAsyncPolledSource !frameQueueLen !src !f = do
  !pq <- mkPayloadQ frameQueueLen
  withAsync
    (runConduit (src .| payloadQSink pq))
    (\a -> f (void a, payloadQSource pq))

data PayloadQ a = MkPayloadQ
  { _payloadQSegmentDuration :: !NominalDiffTime
  , _payloadQPollIntervall :: !NominalDiffTime
  , _payloadQRing :: !(TBQueue a)
  }

mkPayloadQ
  :: forall m a.
     (HasStaticDuration a, MonadBaseControl IO m)
  => Int -> m (PayloadQ a)
mkPayloadQ qlen =
  MkPayloadQ segmentDuration (fromIntegral qlen * 0.5 * segmentDuration) <$>
  liftBase (newTBQueueIO qlen)
  where
    segmentDuration = getStaticDuration (Proxy :: Proxy a)

payloadQSink
  :: (NFData a, MonadBaseControl IO m, Show a)
  => PayloadQ a -> Sink (Stream i s t p a) m ()
payloadQSink (MkPayloadQ _ _ !ringRef) = awaitForever go
  where
    go !x = do
      maybe (return ()) pushInRing (x ^? eachFrameContent)
      return ()
      where
        pushInRing !buf' =
          liftBase $ do
            !buf <- evaluate $ withStrategy rdeepseq buf'
            atomically $ do
              isFull <- isFullTBQueue ringRef
              when isFull (void $ readTBQueue ringRef)
              writeTBQueue ringRef buf

payloadQSource
  :: ( Random i
     , NFData c
     , NFData p
     , Default p
     , HasStaticDuration c
     , HasDuration c
     , MonadBaseControl IO m
     , KnownRate r
     , Integral t
     , Integral s
     , NFData t
     , NFData s
     )
  => PayloadQ c -> Source m (Stream i s (Ticks r t) p (Discontinous c))
payloadQSource (MkPayloadQ pTime pollIntervall ringRef) =
  evalStateC (MkPollPayloadSourceSt 0 0) $ do
    yieldStart
    go False
  where
    go wasMissing = do
      res <- liftBase $ race (atomically $ readTBQueue ringRef) sleep
      case res of
        Left buf -> yieldNextBuffer (Got buf) >> go False
        Right dt -> yieldMissing dt wasMissing >> go True
    sleep =
      liftBase
        (do !(t0 :: ClockTime UtcClock) <- now
            threadDelay (_ticks pollIntervallMicros)
            !t1 <- now
            return (_utcClockTimeDiff (diffTime t1 t0)))
    yieldMissing !dt !wasMissing = do
      unless wasMissing (traceM (printf "*** UNDERFLOW: Missing %s" (show dt)))
      replicateM_ (floor (dt / pTime)) (yieldNextBuffer Missing)
    yieldStart =
      (MkFrameCtx <$> liftBase randomIO <*> use ppTicks <*> use ppSeqNum <*>
       pure def) >>=
      yieldStartFrameCtx
    pollIntervallMicros :: Ticks (Hz 1000000) Int
    pollIntervallMicros = nominalDiffTime # pollIntervall
    yieldNextBuffer !buf = do
      let !bufferDuration = nominalDiffTime # getDuration buf
      !ts <- ppTicks <<+= bufferDuration
      !sn <- ppSeqNum <<+= 1
      frm <- liftBase (evaluate (withStrategy rdeepseq $ MkFrame ts sn buf))
      yieldNextFrame frm
