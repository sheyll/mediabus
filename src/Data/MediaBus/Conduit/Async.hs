-- | Asynchronous execution of conduits. This module contains a set of functions
-- to concurrently execute 'Stream' processing conduits and couple them using
-- 'TBQueue's.
--
-- DEPRECATED
module Data.MediaBus.Conduit.Async
  ( withAsyncPolledSource,
    FrameContentQ (),
    mkFrameContentQ,
    frameContentQSink,
    frameContentQSource,
  )
where

import Conduit
  ( ConduitT,
    MonadIO (..),
    MonadResource,
    MonadUnliftIO,
    Void,
    awaitForever,
    evalStateC,
    runConduit,
    (.|),
  )
import Control.Concurrent (threadDelay)
import Control.Lens (makeLenses, use, (#), (<<+=), (^.), (^?))
import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Control.Monad.State
  ( replicateM_,
    unless,
    void,
    when,
  )
import Control.Parallel.Strategies
  ( NFData,
    rdeepseq,
    withStrategy,
  )
import Data.Default (Default (..))
import Data.MediaBus.Basics.Clock
  ( IsClock (ClockTime, diffTime, now),
    UtcClock,
    utcClockTimeDiff,
  )
import Data.MediaBus.Basics.Ticks
  ( HasDuration (getDuration),
    HasStaticDuration,
    Hz,
    KnownRate,
    Ticks (_ticks),
    getStaticDuration,
    nominalDiffTime,
  )
import Data.MediaBus.Conduit.Stream
  ( yieldNextFrame,
    yieldStartFrameCtx,
  )
import Data.MediaBus.Media.Discontinous (Discontinous (..))
import Data.MediaBus.Media.Stream
  ( EachFramePayload (eachFramePayload),
    Frame (MkFrame),
    FrameCtx (MkFrameCtx),
    Stream,
  )
import Data.Proxy (Proxy (..))
import Data.String (IsString (fromString))
import Data.Time.Clock (NominalDiffTime)
import Numeric.Natural (Natural)
import System.Random (Random, randomIO)
import Text.Printf (printf)
import UnliftIO
  ( Async,
    TBQueue,
    atomically,
    evaluate,
    isFullTBQueue,
    newTBQueueIO,
    race,
    readTBQueue,
    withAsync,
    writeTBQueue,
  )

data PollFrameContentSourceSt s t = MkPollFrameContentSourceSt
  { _ppSeqNum :: !s,
    _ppTicks :: !t
  }

makeLenses ''PollFrameContentSourceSt

-- | Asynchronously run a source 'ConduitT' connected to a 'FrameContentQ' and create a
-- new source that consumes the queue by polling periodically from that queue,
-- generating a 'Discontinous' output.
withAsyncPolledSource ::
  ( MonadResource m,
    MonadLogger m,
    MonadUnliftIO m,
    KnownRate r,
    Integral t,
    Integral s,
    Default p,
    HasStaticDuration c,
    HasDuration c,
    NFData c,
    NFData p,
    NFData s,
    NFData t,
    Random i,
    Random t,
    Random s,
    Show c
  ) =>
  Natural ->
  ConduitT () (Stream i s (Ticks r t) p c) m () ->
  ( ( Async (),
      ConduitT () (Stream i s (Ticks r t) p (Discontinous c)) m ()
    ) ->
    m o
  ) ->
  m o
withAsyncPolledSource !frameQueueLen !src !f = do
  !pq <- mkFrameContentQ frameQueueLen
  withAsync
    (runConduit (src .| frameContentQSink pq))
    (\a -> f (void a, frameContentQSource pq))

-- | A queue for 'frameContent' to decouple concurrent conduits carrying
-- 'Stream's. Under the hood a 'TBQueue' is used. A queue also knows it's
-- default segment duration and preferred polling interval.
data FrameContentQ a = MkFrameContentQ
  { _frameContentQSegmentDuration :: !NominalDiffTime,
    _frameContentQPollInterval :: !NominalDiffTime,
    _frameContentQRing :: !(TBQueue a)
  }

-- | Create a new 'FrameContentQ' with an upper bound on the queue length.
mkFrameContentQ ::
  forall m a.
  (HasStaticDuration a, MonadIO m) =>
  Natural ->
  m (FrameContentQ a)
mkFrameContentQ qlen =
  MkFrameContentQ segmentDuration (fromIntegral qlen * 0.5 * segmentDuration)
    <$> newTBQueueIO qlen
  where
    segmentDuration = getStaticDuration (Proxy :: Proxy a)

-- | Consume the 'frameContent's of a 'Stream' and write them into a
-- 'FrameContentQ'. When the queue is full, **drop the oldest element** and push
-- in the new element, anyway.
frameContentQSink ::
  (NFData a, Show a, MonadLogger m, MonadIO m) =>
  FrameContentQ a ->
  ConduitT (Stream i s t p a) Void m ()
frameContentQSink (MkFrameContentQ _ _ !ringRef) = awaitForever go
  where
    go !x = do
      maybe (return ()) pushInRing (x ^? eachFramePayload)
      return ()
      where
        pushInRing !buf' = do
          isFull <- do
            !buf <- evaluate $ withStrategy rdeepseq buf'
            atomically $ do
              isFull <- isFullTBQueue ringRef
              when isFull (void $ readTBQueue ringRef)
              writeTBQueue ringRef buf
              return isFull
          when isFull $ $logInfo "queue full"

-- | Periodically poll a 'FrameContentQ' and yield the content as frames with
-- newly generated timestamp and sequence number values.
frameContentQSource ::
  ( Random i,
    NFData c,
    NFData p,
    Default p,
    HasStaticDuration c,
    HasDuration c,
    MonadLogger m,
    MonadIO m,
    KnownRate r,
    Integral t,
    Integral s,
    NFData t,
    NFData s
  ) =>
  FrameContentQ c ->
  ConduitT () (Stream i s (Ticks r t) p (Discontinous c)) m ()
frameContentQSource (MkFrameContentQ pTime pollIntervall ringRef) =
  evalStateC (MkPollFrameContentSourceSt 0 0) $ do
    yieldStart
    go False
  where
    -- TODO this breaks when 'frameContentQPollInterval < duration c'?
    --      to fix add a 'timePassedSinceLastBufferReceived' parameter to 'go'
    --      when no new from could be read from the queue after waiting for 'dt'
    --      seconds, the time waited is added to 'frameContentQPollInterval'
    --      and if 'frameContentQPollIntervall' is greater than the 'duration of c'
    --      a 'Missing' is yielded and 'duration of c' is subtracted from
    --      'timePassedSinceLastBufferReceived'.
    go wasMissing = do
      res <- liftIO $ race (atomically $ readTBQueue ringRef) sleep
      case res of
        Left buf -> yieldNextBuffer (Got buf) >> go False
        Right dt -> yieldMissing dt wasMissing >> go True
    sleep =
      liftIO
        ( do
            !(t0 :: ClockTime UtcClock) <- now
            threadDelay (_ticks pollIntervallMicros)
            !t1 <- now
            return (diffTime t1 t0 ^. utcClockTimeDiff)
        )
    yieldMissing !dt !wasMissing = do
      unless
        wasMissing
        ($logDebug (fromString (printf "underflow: %s" (show dt))))
      replicateM_ (floor (dt / pTime)) (yieldNextBuffer Missing)
    yieldStart =
      ( MkFrameCtx
          <$> liftIO randomIO
          <*> use ppTicks
          <*> use ppSeqNum
          <*> pure def
      )
        >>= yieldStartFrameCtx
    pollIntervallMicros :: Ticks (Hz 1000000) Int
    pollIntervallMicros = nominalDiffTime # pollIntervall
    yieldNextBuffer !buf = do
      let !bufferDuration = nominalDiffTime # getDuration buf
      !ts <- ppTicks <<+= bufferDuration
      !sn <- ppSeqNum <<+= 1
      frm <- evaluate (withStrategy rdeepseq $ MkFrame ts sn buf)
      yieldNextFrame frm
