-- | Make a 'Stream' of media a segmented stream by using that has content which is an instance of 'CanSegment'.
-- TODO move or merge - after deciding howto proceed with the package structure in general
module Data.MediaBus.Conduit.Segment
  ( segmentC,
    forgetSegmentationC,
  )
where

import Conduit (ConduitT, await, awaitForever, evalStateC)
import Control.Lens
  ( makeLenses,
    (.=),
    (<<+=),
    (<<.=),
    (^.),
  )
import Control.Monad (when)
import Data.Default (Default)
import Data.MediaBus.Basics.Series (Series (Next, Start))
import Data.MediaBus.Basics.Ticks
  ( CanBeTicks,
    HasDuration (..),
    Ticks,
  )
import Data.MediaBus.Conduit.Stream
  ( mapFrameContentC,
    yieldNextFrame,
    yieldStartFrameCtx,
  )
import Data.MediaBus.Media.Segment (CanSegment (..), Segment (..))
import Data.MediaBus.Media.Stream
  ( Frame (MkFrame),
    Stream (MkStream),
    frameCtxSeqNumRef,
    frameCtxTimestampRef,
    frameSeqNum,
    frameTimestamp
  )
import Data.Time (NominalDiffTime)

data SegmenterState s t c = SegSt
  { _nextSeq :: s,
    _timeOffset :: t,
    _segRest :: Maybe c
  }

mkSegmenterState :: s -> t -> SegmenterState s t c
mkSegmenterState s t = SegSt s t Nothing

makeLenses ''SegmenterState

-- | The packetizer recombines incoming packets into 'Segment's of the given
-- size. Sequence numbers start with the sequence number of the first frame
-- or the last start frame, the same holds for timestamps.
segmentC ::
  forall i s r t p c m.
  ( Num s,
    Monad m,
    CanSegment c,
    Monoid c,
    Default i,
    HasDuration c,
    CanBeTicks r t
  ) =>
  NominalDiffTime ->
  ConduitT
    (Stream i s (Ticks r t) p c)
    (Stream i s (Ticks r t) p (Segment c))
    m
    ()
segmentC segmentDuration =
  when (segmentDuration > 0) $
    await >>= \case
      Nothing -> return ()
      Just x -> do
        let (seq0, ts0) =
              case x of
                MkStream (Start xStart) ->
                  ( xStart ^. frameCtxSeqNumRef,
                    xStart ^. frameCtxTimestampRef
                  )
                MkStream (Next xNext) ->
                  ( xNext ^. frameSeqNum,
                    xNext ^. frameTimestamp
                  )
        evalStateC (mkSegmenterState seq0 ts0) $ do
          go x
          awaitForever go
          yieldRest
  where
    go (MkStream (Next (MkFrame _ _ !cIn))) =
      yieldLoop cIn
    go (MkStream (Start !frmCtx)) = do
      yieldRest
      let (seq0, ts0) =
            ( frmCtx ^. frameCtxSeqNumRef,
              frmCtx ^. frameCtxTimestampRef
            )
      nextSeq .= seq0
      timeOffset .= ts0
      yieldStartFrameCtx frmCtx
    yieldLoop !cIn = do
      !cRest <- segRest <<.= Nothing
      let !c = maybe cIn (<> cIn) cRest
      if getDuration c == segmentDuration
        then yieldWithAdaptedSeqNumAndTimestamp c
        else case splitAfterDuration segmentDuration c of
          Just (!packet, !rest) -> do
            yieldWithAdaptedSeqNumAndTimestamp packet
            yieldLoop rest
          Nothing ->
            segRest .= Just c
    yieldWithAdaptedSeqNumAndTimestamp !p = do
      !s <- nextSeq <<+= 1
      !t <- timeOffset <<+= getDurationTicks p
      yieldNextFrame (MkFrame t s (MkSegment p))
    yieldRest = do
      !cRest <- segRest <<.= Nothing
      mapM_ yieldWithAdaptedSeqNumAndTimestamp cRest

-- | Simply drop the 'Segment' wrap around the 'Frame' content.
forgetSegmentationC :: (Monad m) => ConduitT (Stream i s t p (Segment c)) (Stream i s t p c) m ()
forgetSegmentationC = mapFrameContentC _segmentContent

-- | Yield each 'Segment' like 'forgetSegmentationC' followed by a 'Start' value if the segment is
-- not the last segment.
--
