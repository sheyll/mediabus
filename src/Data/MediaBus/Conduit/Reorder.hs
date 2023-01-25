-- | Reorder the 'Frame's in 'Conduit' of a 'Stream'.
module Data.MediaBus.Conduit.Reorder
  ( reorderFramesBySeqNumC,
    reorderFramesByC,
  )
where

import Conduit (ConduitT, awaitForever, evalStateC, yield)
import Control.Lens
  ( Lens',
    makeLenses,
    use,
    view,
    (%=),
    (&),
    (.=),
    (.~),
    (<+=),
    (<<.=),
    (^.),
  )
import Control.Monad.State.Strict (MonadState (put), when)
import Data.Default (Default (..))
import Data.MediaBus.Basics.OrderedBy
  ( OrderedBy (MkOrderedBy, orderedByValue),
  )
import Data.MediaBus.Basics.Sequence (HasSeqNum (seqNum))
import Data.MediaBus.Basics.Series (Series (Start))
import Data.MediaBus.Media.Stream (Stream (MkStream))
import qualified Data.Set as Set

data ReorderSt a b c = MkReorderSt
  { _expectedRank :: !a,
    _frameQueue :: !(Set.Set (OrderedBy a b)),
    _frameDrops :: !Int,
    _lastFrameCtx :: !c
  }

makeLenses ''ReorderSt

-- | Reorder the 'Frame's in 'Conduit' of a 'Stream' according to the 'Ord'
-- instance of the the sequence numbers of 'seqNum'. This function will buffer e
-- certain number of frames that are out of order, and drops frames if they are
-- too late. Also, when too many consecutive frames have all been dropped, a new
-- 'Start' will be created, and the buffered elements are silently dropped, too.
-- When a 'Start' is received that internal buffer is flushed and all queued
-- frames are transmitted.
reorderFramesBySeqNumC ::
  (Default s, Default i, Default t, Default p, Num s, Ord s, Monad m) =>
  -- | The maximun number of out-of-order frames to buffer.
  Int ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
reorderFramesBySeqNumC = reorderFramesByC seqNum (+ 1)

-- | Like 'reorderFramesBySeqNumC' but more general. This function allows to
-- pass a 'Lens' to the specific field of each 'Frame', that shall be used for
-- comparison, which governs the order.
reorderFramesByC ::
  ( Monad m,
    Ord rank,
    Default i,
    Default t,
    Default s,
    Default p,
    Default rank
  ) =>
  -- | A lens for the value to be used as comparison
  Lens' (Stream i s t p c) rank ->
  -- | A function that returns the **expected next value** of the comparison value
  (rank -> rank) ->
  -- | The maximum number of frames to buffer
  Int ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
reorderFramesByC !frameRank !getNextRank !maxQueueLen =
  evalStateC (MkReorderSt def Set.empty 0 def) go
  where
    maxDrops = maxQueueLen
    go = do
      awaitForever handleNext
      flushQueue
      where
        handleNext s@(MkStream (Start !ctx)) = do
          flushQueue
          yield s
          put (MkReorderSt (s ^. frameRank) Set.empty 0 ctx)
        handleNext !frm = do
          !expRank <- use expectedRank
          let !currRank = frm ^. frameRank
          case compare currRank expRank of
            EQ -> do
              yieldNext frm
              maybeYieldNextFromQueue
            LT ->
              -- drop the frame, it lacks behind
              do
                framesDropped <- frameDrops <+= 1
                when (framesDropped == maxDrops) $ do
                  flushQueue
                  -- yield a new Start frame
                  ctx <- use lastFrameCtx
                  let start = MkStream (Start ctx) & frameRank .~ (frm ^. frameRank)
                      MkStream (Start ctx') = start
                  lastFrameCtx .= ctx'
                  yield start
                  yieldNext frm
            GT -> do
              frameQueue %= Set.insert (MkOrderedBy (view frameRank frm) frm)
              maybeYieldNextFromQueue
        yieldNext !frm = do
          expectedRank .= frm ^. frameRank
          updateExpectedRank
          frameDrops .= 0
          yield frm
        flushQueue = do
          !q <- frameQueue <<.= Set.empty
          mapM_ (yieldNext . orderedByValue) (Set.toAscList q)
        maybeYieldNextFromQueue = do
          !q <- use frameQueue
          !expRank <- use expectedRank
          case Set.minView q of
            Nothing -> return ()
            Just (MkOrderedBy !currRank !candidate, !q') ->
              let !isQueueFull = Set.size q == maxQueueLen
                  !isNextInQueue = currRank <= expRank
               in when (isQueueFull || isNextInQueue) $ do
                    frameQueue .= q'
                    yieldNext candidate
                    maybeYieldNextFromQueue
        updateExpectedRank = expectedRank %= getNextRank
