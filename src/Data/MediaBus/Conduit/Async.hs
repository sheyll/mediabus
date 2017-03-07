-- | Asynchronous execution of conduits. This module contains a set of functions
-- to concurrently execute 'Stream' processing conduits and couple them using
-- 'TBQueue's.
module Data.MediaBus.Conduit.Async
  ( withAsyncPolledSource
  , FrameContentQ()
  , mkFrameContentQ
  , frameContentQSink
  , frameContentQSource
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
import Data.MediaBus.Conduit.Stream
import Data.Proxy
import Data.Time.Clock
import Debug.Trace
import System.Random
import Text.Printf

data PollFrameContentSourceSt s t = MkPollFrameContentSourceSt
  { _ppSeqNum :: !s
  , _ppTicks :: !t
  }

makeLenses ''PollFrameContentSourceSt

-- | Asynchronously run a 'Source' connected to a 'FrameContentQ' and create a
-- new source that consumes the queue by polling periodically from that queue,
-- generating a 'Discontinous' output.
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
  !pq <- mkFrameContentQ frameQueueLen
  withAsync
    (runConduit (src .| frameContentQSink pq))
    (\a -> f (void a, frameContentQSource pq))

-- | A queue for 'frameContent' to decouple concurrent conduits carrying
-- 'Stream's. Under the hood a 'TBQueue' is used. A queue also knows it's
-- default segment duration and preferred polling interval.
data FrameContentQ a = MkFrameContentQ
  { _frameContentQSegmentDuration :: !NominalDiffTime
  , _frameContentQPollInterval :: !NominalDiffTime
  , _frameContentQRing :: !(TBQueue a)
  }

-- | Create a new 'FrameContentQ' with an upper bound on the queue length.
mkFrameContentQ
  :: forall m a.
     (HasStaticDuration a, MonadBaseControl IO m)
  => Int -> m (FrameContentQ a)
mkFrameContentQ qlen =
  MkFrameContentQ segmentDuration (fromIntegral qlen * 0.5 * segmentDuration) <$>
  liftBase (newTBQueueIO qlen)
  where
    segmentDuration = getStaticDuration (Proxy :: Proxy a)

-- | Consume the 'frameContent's of a 'Stream' and write them into a
-- 'FrameContentQ'. When the queue is full, **drop the oldest element** and push
-- in the new element, anyway.
frameContentQSink
  :: (NFData a, MonadBaseControl IO m, Show a)
  => FrameContentQ a -> Sink (Stream i s t p a) m ()
frameContentQSink (MkFrameContentQ _ _ !ringRef) = awaitForever go
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

-- | Periodically poll a 'FrameContentQ' and yield the content as frames with
-- newly generated timestamp and sequence number values.
frameContentQSource
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
  => FrameContentQ c -> Source m (Stream i s (Ticks r t) p (Discontinous c))
frameContentQSource (MkFrameContentQ pTime pollIntervall ringRef) =
  evalStateC (MkPollFrameContentSourceSt 0 0) $ do
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
            return (diffTime t1 t0 ^. utcClockTimeDiff))
    yieldMissing !dt !wasMissing = do
      unless wasMissing (traceM (printf "*** UNDERFLOW: Missing %s" (show dt))) -- TODO replace by proper logging
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
