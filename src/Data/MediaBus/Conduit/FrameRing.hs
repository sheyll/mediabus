-- | Asynchronous execution of conduits. This module contains a set of functions
-- to concurrently execute 'Stream' processing conduits and couple them using
-- 'TBQueue's.
module Data.MediaBus.Conduit.FrameRing
  ( FrameRing (),
    mkFrameRing,
    frameRingSink,
    frameRingSource,
  )
where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Parallel.Strategies
  ( NFData,
    rdeepseq,
    withStrategy,
  )
import Data.Default
import Data.Fixed (Fixed (..))
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream
import Data.MediaBus.Media.SyncStream
import Data.Time.Clock
import Numeric.Natural
import System.Random
import UnliftIO
import Control.Concurrent.STM (flushTBQueue)

data RingSourceState s t = MkRingSourceState
  { _lastWakeupTime :: !UTCTime,
    _nextYieldTime :: !NominalDiffTime
  }

makeLenses ''RingSourceState

-- | A queue, that decouples content generation and consumption
-- such that two threads can simultanously produce and consume
-- frames.
--
-- The implementation uses bounded queues 'TBQueue'.
--
-- Refer to 'frameRingSink' to learn howto put data into the ring
-- and 'frameRingSource' on how to retreive data.
newtype FrameRing i p c = MkFrameRing
  { _frameRingTBQueue :: TBQueue (SyncStream i p c)
  }

-- | Create a new 'FrameRing' with an upper bound on the queue length.
mkFrameRing ::
  (MonadIO m, Random i, Default p) =>
  Natural ->
  m (FrameRing i p c)
mkFrameRing qlen =
  do
    r <- newTBQueueIO qlen
    !ssrc <- randomIO
    atomically (writeTBQueue r (MkStream (Start (MkFrameCtx ssrc def def def))))
    return (MkFrameRing r)

-- | Consume a 'SyncStream' and put each value into a 'FrameRing'.
-- When the ring is full, **drop the oldest** element that is not a
-- 'Start' value, unless the second oldest is also a 'Start' value.
-- If the queue length is 1 then the new value will not be written
-- as long a the 'Start' value is not retreived.
frameRingSink ::
  (NFData c, NFData i, NFData p, MonadIO m) =>
  FrameRing i p c ->
  ConduitT (SyncStream i p c) Void m ()
frameRingSink (MkFrameRing !ringRef) = awaitForever pushInRing
  where
    pushInRing !newest' = do
      newest <- evaluate $ withStrategy rdeepseq newest'
      atomically $ do
        isFull <- isFullTBQueue ringRef
        if isFull
          then do
            oldest <- readTBQueue ringRef
            case oldest of
              MkStream (Start _) -> do
                isEmpty <- isEmptyTBQueue ringRef
                if isEmpty
                  then writeTBQueue ringRef $
                    case newest of
                      MkStream (Next _) ->
                        oldest -- drop the new frame
                      MkStream (Start _) ->
                        newest -- drop the old frame
                  else do
                    secondOldest <- readTBQueue ringRef
                    case secondOldest of
                      MkStream (Start _) ->
                        unGetTBQueue ringRef secondOldest
                      MkStream (Next _) ->
                        unGetTBQueue ringRef oldest
                    writeTBQueue ringRef newest
              MkStream (Next _) ->
                writeTBQueue ringRef newest
          else
            case newest of
              MkStream (Next _) ->
                writeTBQueue ringRef newest
              MkStream (Start newestStart) -> do
                prevs <- flushTBQueue ringRef
                case reverse prevs of
                  [] ->
                    writeTBQueue ringRef newest
                  (MkStream (Start _):rest) -> do
                    mapM_ (writeTBQueue ringRef) rest
                    writeTBQueue ringRef newest
                    -- if the most recent and the previous values are both start frames
                    -- replace the previous value
                  _ -> do
                    mapM_ (unGetTBQueue ringRef) prevs
                    unGetTBQueue ringRef newest


-- | Periodically poll a 'FrameRing' and yield the 'Frame's
-- put into the ring, or 'Missing' otherwise.
--
-- The output of the conduit is a 'SyncStream', i.e. a stream
-- without sequence number and timestamps.
frameRingSource ::
  forall i p c m.
  (MonadIO m, HasDuration c, Random i, Default p) =>
  FrameRing i p c ->
  NominalDiffTime ->
  ConduitT () (SyncStream i p (Discontinous c)) m ()
frameRingSource (MkFrameRing !ringRef) !defaultPacketDuration = do
  !tStart <- liftIO getCurrentTime
  go tStart (MkRingSourceState tStart 0)
  where
    go !tStart !st = do
      let !tNext = addUTCTime (st ^. nextYieldTime) tStart
      let !tLast = st ^. lastWakeupTime
      let (MkFixed !dPicos) = nominalDiffTimeToSeconds (max (diffUTCTime tNext tLast) minJitter)
          !dMicros = fromIntegral (dPicos `div` 1000000)
      liftIO (threadDelay dMicros)
      tWakeup <- liftIO getCurrentTime
      let !dtWakeup = diffUTCTime tNext tWakeup
      let !isEarlyWakeup = dtWakeup > maxJitter
      let !st' = st & lastWakeupTime .~ tWakeup
      if isEarlyWakeup
        then go tStart st'
        else do
          !currentPacketDuration <- nextFrame
          let st'' = st' & nextYieldTime +~ currentPacketDuration
          go tStart st''

    nextFrame = do
      !bufs <- liftIO $ atomically $ tryReadTBQueue ringRef
      case bufs of
        Just (MkStream !buf) ->
          case buf of
            Next (MkFrame _ _ x) -> do
              yieldNextFrame (MkFrame def def (Got x))
              return (getDuration x)
            Start (MkFrameCtx ssrc _ _ p) -> do
              -- Start frame
              yieldStartFrameCtx (MkFrameCtx ssrc def def p)
              return 0
        Nothing -> do
          yieldNextFrame (MkFrame def def Missing)
          return missingDuration

    minJitter, maxJitter, missingDuration :: NominalDiffTime
    minJitter = defaultPacketDuration / 4
    maxJitter = defaultPacketDuration / 2
    missingDuration = defaultPacketDuration
