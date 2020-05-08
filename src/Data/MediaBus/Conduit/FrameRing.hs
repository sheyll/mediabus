-- | This module provides the 'FrameRing' data structure.
--
-- The ring that acts like a /triple-buffer/ knwon from
-- traditional graphics programming.
--
-- It connects a producer to a consumer via a 'TBQueue',
-- and allows to use different /threads/.
--
-- If the producer lags behind, 'Missing' frames will be issued.
--
-- @since 0.5.0.0
module Data.MediaBus.Conduit.FrameRing
  ( FrameRing (),
    Event(..),
    mkFrameRing,
    frameRingSink,
    frameRingSource,
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
import Data.MediaBus.Basics.Sequence
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.SyncStream
import Data.MediaBus.Media.Stream
import Data.Proxy
import Data.String
import Data.Time.Clock
import Numeric.Natural
import System.Random
import Text.Printf
import UnliftIO

-- | A ring like queue, to provide a constant flow of frames.
--
-- This helps to decouple concurrent conduits carrying
-- 'Stream's.
--
-- The implementation uses bounded queues 'TBQueue'.
--
-- The internal queue can be filled from one thread and consumed by
-- another thread.
--
-- Refer to 'frameRingSink' to learn howto put data into the ring
-- and 'frameRingSource' on how to retreive data.
newtype FrameRing a
  = MkFrameRing
      { _frameRingTBQueue
            :: TBQueue
                (SinkOverflow a,
                 a
                )
      }

-- | Internal helper
newtype SinkOverflow a =
    SinkOverflow { overwrittenPayload :: Maybe a }

-- | A 'FrameRing' overflow or underflow event.
--
-- @since 0.5.0.0
data Event c =
    Overflow { lostPayload :: c}
    -- ^ The ring was too full and the first entry was overwritten by the next.
  | Underflow
    -- ^ The ring was empty for too long.

-- | Create a new 'FrameRing' with an upper bound on the queue length.
mkFrameRing ::
  (MonadIO m) =>
  Natural ->
  -- ^ Ring Element Count
  m (FrameRing a)
mkFrameRing qlen =
  MkFrameRing <$> newTBQueueIO qlen

-- | Consume the 'Frame's of a 'Stream' and write them into a
-- 'FrameRing'. When the queue is full, **drop the oldest element** and push
-- in the new element, anyway.
frameRingSink ::
  (NFData sourceId, NFData streamStartPayload, NFData payload, MonadIO m) =>
  FrameRing (SyncStream sourceId streamStartPayload payload) ->
  ConduitT (SyncStream sourceId streamStartPayload payload) Void m ()
frameRingSink (MkFrameRing !ringRef) = awaitForever pushInRing
  where
    pushInRing !buf' = do
      !buf <- evaluate (withStrategy rdeepseq buf')
      atomically (do
        isFull <- isFullTBQueue ringRef
        overflow <-
          if isFull then
            SinkOverflow . Just . snd
                <$> readTBQueue ringRef
          else return (SinkOverflow Nothing)

        writeTBQueue ringRef (overflow, buf))


-- | Internal helper.
data SourceState = MkSourceState

-- | Periodically poll a 'FrameRing' and yield the 'Frame's
-- put into the ring, or 'Missing' otherwise.
--
-- Pass frames through.
-- If a start frame is received, pass it through.
-- If no frames are received for
frameRingSource :: forall m sourceId streamStartPayload payload.
  ( MonadIO m, HasStaticDuration payload ) =>
  FrameRing (SyncStream sourceId streamStartPayload payload) ->
  ConduitT () (SyncStream sourceId streamStartPayload (SinkOverflow  payload)) m ()
  --  TODO ConduitT () (Stream i s (Ticks r t) p (AnnotatedFrame (Maybe FrameRingEvent) (Discontinous c))) m ()
frameRingSource  (MkFrameRing ringRef) =
  do
    yieldStart
    go
  where
    pTime = getStaticDuration (Proxy @payload)

    pollIntervallMicros :: Ticks (Hz 1000000) Int
    pollIntervallMicros = nominalDiffTime # pTime

    -- TODO this breaks when 'frameRingPollInterval < duration c'?
    --      to fix add a 'timePassedSinceLastBufferReceived' parameter to 'go'
    --      when no new from could be read from the queue after waiting for 'dt'
    --      seconds, the time waited is added to 'frameRingPollInterval'
    --      and if 'frameRingPollIntervall' is greater than the 'duration of c'
    --      a 'Missing' is yielded and 'duration of c' is subtracted from
    --      'timePassedSinceLastBufferReceived'.
    go = do
      res <- liftIO $ race (atomically $ readTBQueue ringRef) sleep
      case res of
        Left buf -> do
          yieldNextBuffer (Got buf)
        Right dt -> do
          yieldMissing
      go

    sleep =
      liftIO
        ( do
            !(t0 :: ClockTime UtcClock) <- now
            threadDelay (_ticks pollIntervallMicros)
            !t1 <- now
            return (diffTime t1 t0 ^. utcClockTimeDiff)
        )
        where
    yieldStart =
      error "TODO" >>= yieldStartFrameCtx

    yieldNextBuffer !buf =
      yieldNextFrame (error "TODO 2")


    yieldMissing =
      yieldNextBuffer Missing
