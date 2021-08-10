{-# LANGUAGE TupleSections #-}
-- | Asynchronous execution of conduits. This module contains a set of functions
-- to concurrently execute 'Stream' processing conduits and couple them using
-- an array of buffers, forming a ring, which means that when
-- the array is full, the writing thread overwrites existing elements.
--
-- Both reading and writing only block when accessing the same ring buffer
-- element at the same time.
--
-- When clear on read is active the reader replaces the element just read with
-- blank media.
module Data.MediaBus.Conduit.FrameRing
  ( FrameRing (),
    mkFrameRing,
    frameRingSink,
    frameRingSource,
    withConcurrentSource
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
import qualified Data.Vector as V
import Control.Monad (void)

data RingSourceState s t = MkRingSourceState
  { _lastWakeupTime :: !UTCTime,
    _nextYieldTime :: !NominalDiffTime,
    _readPos :: !Int
  }

makeLenses ''RingSourceState

-- | Asynchronously run a source 'ConduitT' connected to a 'FrameRing' and create a
-- new source that consumes the queue by polling periodically from that queue,
-- generating a 'Discontinous' output.
withConcurrentSource ::
  forall i p c m o.
  ( MonadResource m,
    MonadUnliftIO m,
    Default p,
    HasStaticDuration c,
    HasDuration c,
    NFData c,
    NFData p,
    NFData i,
    Random i
  ) =>
  Natural ->
  ConduitT () (SyncStream i p c) m () ->
  ( ( Async (),
      ConduitT () (SyncStream i p (Discontinous c)) m ()
    ) ->
    m o
  ) ->
  m o
withConcurrentSource !frameQueueLen !src !f = do
  !pq <- mkFrameRing frameQueueLen
  let
    defaultPacketDuration = getStaticDuration (Nothing @c)
  withAsync
    (runConduit (src .| frameRingSink pq))
    (\a -> f (void a, frameRingSource pq defaultPacketDuration))


-- | A queue, that decouples content generation and consumption
-- such that two threads can simultanously produce and consume
-- frames.
--
-- The implementation uses bounded queues 'TBQueue'.
--
-- Refer to 'frameRingSink' to learn howto put data into the ring
-- and 'frameRingSource' on how to retreive data.
newtype FrameRing i p c = MkFrameRing
  { _frameRingArray :: V.Vector (IORef (Maybe (SyncStream i p c)))
  }

-- | Create a new 'FrameRing' with an upper bound on the queue length.
mkFrameRing ::
  (MonadIO m, Random i, Default p) =>
  Natural ->
  m (FrameRing i p c)
mkFrameRing qlen' =
  do
    -- the ring size must be >= 2
    let qlen = max 2 qlen'

    -- create an array of IORefs containig 'Missing'
    ring <- V.replicateM (fromIntegral qlen) (newIORef Nothing)

    -- let the first element be a 'Start' frame with a random stream id
    !ssrc <- randomIO
    atomicWriteIORef (ring V.! 0) (Just (MkStream (Start (MkFrameCtx ssrc def def def))))

    return (MkFrameRing ring)

-- | Consume a 'SyncStream' and put each value into a 'FrameRing'.
-- When the ring is full, **drop the oldest** element that is not a
-- 'Start' value, unless the second oldest is also a 'Start' value.
-- If the queue length is 1 then the new value will not be written
-- as long a the 'Start' value is not retreived.
frameRingSink ::
  (NFData c, NFData i, NFData p, MonadIO m) =>
  FrameRing i p c ->
  ConduitT (SyncStream i p c) Void m ()
frameRingSink (MkFrameRing !ringRef) = do
  writeIndexRef <- newIORef 1
  awaitForever (pushInRing writeIndexRef)
  where
    ringSize = V.length ringRef

    pushInRing !writeIndexRef !newest' = do
      -- read and update the next write position
      !writePos <- atomicModifyIORef' writeIndexRef (\i -> ((i + 1) `mod` ringSize, i))
      -- Debug.traceM (printf "writing to: %i\n" writePos)

      -- strictly evaluate the incoming element
      newest <- evaluate $ withStrategy rdeepseq newest'

      -- replace the ring buffer element at writePos
      void $ atomicModifyIORef
              (ringRef V.! writePos)
              (\mOldest -> (Just (replaceRingElem mOldest newest), ()))

    replaceRingElem !mOldest !newest =
      case mOldest of
        Nothing ->
          newest -- if the ring buffer element was read,
                 -- write the newest frame
        Just oldest ->
          case newest of
            MkStream (Start _) -> -- start frames are always written
              newest
            _ ->
                  -- the oldest element wasn't read yet
                  case oldest of
                    MkStream (Next _) ->
                      -- replace with newest
                      newest
                    MkStream (Start _) ->
                      -- an old, unread start value is more important
                      -- than a new payload frame
                      oldest

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
  go tStart (MkRingSourceState tStart 0 0)
  where
    ringSize = V.length ringRef

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
          (!nextReadPos, !currentPacketDuration) <- nextFrame (st ^. readPos)
          let st'' = st' & nextYieldTime +~ currentPacketDuration
                         & readPos .~ nextReadPos
          go tStart st''

    nextFrame rp = do
      !bufs <- atomicModifyIORef (ringRef V.! rp) (Nothing,)
      case bufs of
        Just (MkStream !buf) ->
          let
            nextPos = (rp + 1) `mod` ringSize
          in
            case buf of
              Next (MkFrame _ _ x) -> do
                -- Debug.traceM (printf "read payload from: %i\n" rp)
                yieldNextFrame (MkFrame def def (Got x))
                return (nextPos, getDuration x)
              Start (MkFrameCtx ssrc _ _ p) -> do
                -- Start frame
                -- Debug.traceM (printf "read start from: %i\n" rp)
                yieldStartFrameCtx (MkFrameCtx ssrc def def p)
                return (nextPos, 0)
        Nothing -> do
          -- Debug.traceM (printf "read nothing from: %i\n" rp)
          yieldNextFrame (MkFrame def def Missing)
          return (rp, missingDuration)

    minJitter, maxJitter, missingDuration :: NominalDiffTime
    minJitter = defaultPacketDuration / 4
    maxJitter = defaultPacketDuration / 2
    missingDuration = defaultPacketDuration
