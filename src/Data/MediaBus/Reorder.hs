{-# OPTIONS -Wno-unused-top-binds #-}
module Data.MediaBus.Reorder
    ( reorderFramesBySeqNumC
    , reorderFramesByC
    ) where

import           Data.MediaBus.Stream
import           Data.MediaBus.Basics.Sequence
import           Data.MediaBus.Basics.OrderedBy
import           Data.MediaBus.Basics.Series
import qualified Data.Set                         as Set
import           Conduit
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Function                    ( on )
import           Data.Default

data ReorderSt a b c = MkReorderSt { _expectedRank :: !a
                                   , _frameQueue   :: !(Set.Set (OrderedBy b))
                                   , _frameDrops   :: !Int
                                   , _lastFrameCtx :: !c
                                   }

makeLenses ''ReorderSt

reorderFramesBySeqNumC :: (Default s, Default i, Default t, Default p, Num s, Ord s, Monad m)
                       => Int
                       -> Conduit (Stream i s t p c) m (Stream i s t p c)
reorderFramesBySeqNumC =
    reorderFramesByC seqNum (+ 1)

reorderFramesByC :: (Monad m, Ord rank, Default i, Default t, Default s, Default p, Default rank)
                 => Lens' (Stream i s t p c) rank
                 -> (rank -> rank)
                 -> Int
                 -> Conduit (Stream i s t p c) m (Stream i s t p c)
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
                LT -> do
                    -- drop the frame, it lacks behind
                    framesDropped <- frameDrops <+= 1
                    when (framesDropped == maxDrops) $ do
                        flushQueue
                        -- yield a new Start frame
                        ctx <- use lastFrameCtx
                        let start = MkStream (Start ctx) & frameRank .~
                                (frm ^. frameRank)
                            MkStream (Start ctx') = start
                        lastFrameCtx .= ctx'
                        yield start
                        yieldNext frm

                GT -> do
                    frameQueue %= Set.insert (MkOrderedBy rankCmp frm)
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
                Just (MkOrderedBy _ !candidate, !q') ->
                    let !currRank = candidate ^. frameRank
                        !isQueueFull = Set.size q == maxQueueLen
                        !isNextInQueue = currRank <= expRank
                    in
                        when (isQueueFull || isNextInQueue) $ do
                            frameQueue .= q'
                            yieldNext candidate
                            maybeYieldNextFromQueue

        updateExpectedRank = expectedRank %= getNextRank

        rankCmp = compare `on` view frameRank
