-- | Make a 'Stream' of media a segmented stream by using that has content which is an instance of 'CanSegment'.
-- TODO move or merge - after deciding howto proceed with the package structure in general
module Data.MediaBus.Conduit.StaticSegment
  ( staticSegmentC,
    staticSegmentC',
    forgetStaticSegmentationC,
  )
where

import Conduit (ConduitT, awaitForever, evalStateC)
import Control.Lens
  ( Field1 (_1),
    Field2 (_2),
    use,
    (#),
    (+=),
    (-=),
    (.=),
    (<<.=),
  )
import Control.Monad (when)
import Control.Parallel.Strategies (NFData)
import Data.Default (Default)
import Data.MediaBus.Basics.Series (Series (Next, Start))
import Data.MediaBus.Basics.Ticks
  ( CanBeTicks,
    HasDuration (..),
    HasStaticDuration,
    Ticks,
    getStaticDuration,
    nominalDiffTime,
  )
import Data.MediaBus.Conduit.Stream
  ( mapFrameContentC',
    yieldNextFrame,
    yieldStartFrameCtx,
  )
import Data.MediaBus.Media.Segment (CanSegment (..))
import Data.MediaBus.Media.StaticSegment (StaticSegment (..))
import Data.MediaBus.Media.Stream
  ( Frame (MkFrame),
    Stream (MkStream),
  )
import Data.Proxy (Proxy (Proxy))

-- | The packetizer recombines incoming packets into 'Segment's of the given
-- size. The sequence numbers will be offsetted by the number extra frames
-- generated.
staticSegmentC ::
  ( Num s,
    Monad m,
    CanSegment c,
    Monoid c,
    Default i,
    CanBeTicks r t,
    HasDuration c,
    HasStaticDuration d
  ) =>
  ConduitT
    (Stream i s (Ticks r t) p c)
    (Stream i s (Ticks r t) p (StaticSegment d c))
    m
    ()
staticSegmentC = staticSegmentC' Proxy

staticSegmentC' ::
  ( Num s,
    Monad m,
    CanSegment c,
    Monoid c,
    Default i,
    CanBeTicks r t,
    HasDuration c,
    HasStaticDuration d
  ) =>
  proxy d ->
  ConduitT
    (Stream i s (Ticks r t) p c)
    (Stream i s (Ticks r t) p (StaticSegment d c))
    m
    ()
staticSegmentC' dpx =
  when (segmentDuration > 0) $
    evalStateC (0, Nothing) $ awaitForever go
  where
    segmentDurationInTicks = nominalDiffTime # segmentDuration
    segmentDuration = getStaticDuration dpx
    go (MkStream (Next (MkFrame !t !s !cIn))) = do
      !cRest <- _2 <<.= Nothing
      let tsOffset = negate (getDurationTicks cRest)
      !cRest' <- yieldLoop (maybe cIn (<> cIn) cRest) tsOffset
      _2 .= cRest'
      where
        yieldLoop !c !timeOffset =
          if getDuration c == segmentDuration
            then do
              yieldWithAdaptedSeqNumAndTimestamp (MkStaticSegment c)
              return Nothing
            else case splitAfterDuration segmentDuration c of
              Just (!packet, !rest) -> do
                yieldWithAdaptedSeqNumAndTimestamp (MkStaticSegment packet)
                _1 += 1
                yieldLoop rest (timeOffset + segmentDurationInTicks)
              Nothing -> do
                -- we just swallowed an incoming packet, therefore we need
                -- to decrease the seqnums
                -- TODO NO!!!!
                _1 -= 1
                return (Just c)
          where
            yieldWithAdaptedSeqNumAndTimestamp !p = do
              !seqNumOffset <- use _1
              yieldNextFrame (MkFrame (t + timeOffset) (s + seqNumOffset) p)
    go (MkStream (Start !frmCtx)) = yieldStartFrameCtx frmCtx

forgetStaticSegmentationC ::
  (NFData c, Monad m) =>
  ConduitT (Stream i s t p (StaticSegment d c)) (Stream i s t p c) m ()
forgetStaticSegmentationC = mapFrameContentC' _staticSegmentContent
