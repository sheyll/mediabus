-- | Asynchronous execution of conduits. This module contains a set of functions
-- to concurrently execute 'Stream' processing conduits and couple them using
-- 'TBQueue's.
module Data.MediaBus.Conduit.RingBuffer
  ( RingBuffer (),
    mkRingBuffer,
    ringBufferSink,
    ringBufferSource,
  )
where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Logger
import Control.Monad.State
import Control.Parallel.Strategies
  ( NFData,
    rdeepseq,
    withStrategy,
  )
import Data.Default
import Data.MediaBus.Basics.Clock
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream
import Data.Proxy
import Data.String
import Data.Time.Clock
import Numeric.Natural
import System.Random
import Text.Printf
import UnliftIO

data RingBufferState s t
  = MkRingBufferState
      { _ppSeqNum :: !s,
        _ppTicks :: !t
      }

makeLenses ''RingBufferState

-- | A ring like queue, to provide a constant flow of frames.
--
-- This helps to decouple concurrent conduits carrying
-- 'Stream's.
--
-- The implementation uses bounded queues 'TBQueue'.
--
-- The ring buffer relies on a fixed segment duration.
--
-- The internal queue can be filled from one thread and consumed by
-- another thread.
--
-- Refer to 'ringBufferSink' to learn howto put data into the ring
-- and 'ringBufferSource' on how to retreive data.
data RingBuffer a
  = MkRingBuffer
      { _ringBufferSegmentDuration :: !NominalDiffTime,
        _ringBufferPollInterval :: !NominalDiffTime,
        _ringBufferRing :: !(TBQueue a)
      }

-- | Create a new 'RingBuffer' with an upper bound on the queue length.
mkRingBuffer ::
  forall m a.
  (HasStaticDuration a, MonadIO m) =>
  Natural ->
  m (RingBuffer a)
mkRingBuffer qlen =
  MkRingBuffer segmentDuration (fromIntegral qlen * 0.5 * segmentDuration)
    <$> newTBQueueIO qlen
  where
    segmentDuration = getStaticDuration (Proxy :: Proxy a)

-- | Consume the 'frameContent's of a 'Stream' and write them into a
-- 'RingBuffer'. When the queue is full, **drop the oldest element** and push
-- in the new element, anyway.
ringBufferSink ::
  (NFData a, Show a, MonadLogger m, MonadIO m) =>
  RingBuffer a ->
  ConduitT (Stream i s t p a) Void m ()
ringBufferSink (MkRingBuffer _ _ !ringRef) = awaitForever go
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

-- | Periodically poll a 'RingBuffer' and yield the content as frames with
-- newly generated timestamp and sequence number values.
ringBufferSource ::
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
  RingBuffer c ->
  ConduitT () (Stream i s (Ticks r t) p (Discontinous c)) m ()
ringBufferSource (MkRingBuffer pTime pollIntervall ringRef) =
  evalStateC (MkRingBufferState 0 0) $ do
    yieldStart
    go False
  where
    -- TODO this breaks when 'ringBufferPollInterval < duration c'?
    --      to fix add a 'timePassedSinceLastBufferReceived' parameter to 'go'
    --      when no new from could be read from the queue after waiting for 'dt'
    --      seconds, the time waited is added to 'ringBufferPollInterval'
    --      and if 'ringBufferPollIntervall' is greater than the 'duration of c'
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
        ($logDebug (fromString (printf "underflow: %s" (show dt)))) -- TODO add newtype FrameFromRingBuffer x, with an UnderflowAt clause and return that instead of logging
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
