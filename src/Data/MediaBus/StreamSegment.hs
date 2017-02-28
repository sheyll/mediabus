module Data.MediaBus.StreamSegment
    ( segmentC
    , segmentC'
    ) where

import           Conduit
import           Data.Monoid
import           Data.MediaBus.Stream
import           Data.MediaBus.Ticks
import           Data.MediaBus.Segment
import           Control.Lens
import           Data.Default
import           Data.MediaBus.Series
import           Data.Proxy
import           GHC.TypeLits

-- | The packetizer recombines incoming packets into 'Segment's of the given
-- size. The sequence numbers will be offsetted by the number extra frames
-- generated.
segmentC :: (Num s, Monad m, HasDuration c, CanSegment c, Monoid c, Default i, KnownNat r, Integral t, HasStaticDuration d)
         => Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r t) p (Segment d c))
segmentC = segmentC' Proxy

segmentC' :: (Num s, Monad m, HasDuration c, CanSegment c, Monoid c, Default i, KnownNat r, Integral t, HasStaticDuration d)
          => proxy d
          -> Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r t) p (Segment d c))
segmentC' dpx = evalStateC (0, Nothing) $ awaitForever go
  where
    segmentDurationInTicks =
        nominalDiffTime # segmentDuration
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
                yieldWithAdaptedSeqNumAndTimestamp (MkSegment c)
                return Nothing
            else case splitAfterDuration dpx c of
                Just (!packet, !rest) -> do
                    yieldWithAdaptedSeqNumAndTimestamp packet
                    _1 += 1
                    yieldLoop rest (timeOffset + segmentDurationInTicks)
                Nothing -> do
                    -- we just swallowed an incoming packet, therefore we need
                    -- to decrease the seqnums
                    _1 -=
                        1
                    return (Just c)
          where
            yieldWithAdaptedSeqNumAndTimestamp !p = do
                !seqNumOffset <- use _1
                yieldNextFrame (MkFrame (t + timeOffset) (s + seqNumOffset) p)
    go (MkStream (Start !frmCtx)) =
        yieldStartFrameCtx frmCtx
