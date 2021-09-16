{-# LANGUAGE UndecidableInstances #-}

-- | Media segments with a fixed duration
module Data.MediaBus.Media.Segment
  ( Segment (..),
    segmentContent,
    CanSegment (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Lens (Iso, iso)
import Data.Default (Default)
import Data.MediaBus.Basics.Ticks
  ( CoerceRate (..),
    HasDuration (getDuration),
    HasRate (..),
    KnownRate,
  )
import Data.MediaBus.Media.Channels (EachChannel (..))
import Data.MediaBus.Media.Media (HasMedia (..))
import Data.MediaBus.Media.Samples (EachSample (..))
import Data.Time (NominalDiffTime)
import Test.QuickCheck (Arbitrary)
import Text.Printf (printf)
import Data.MediaBus.Media.Buffer

-- | A segment is some content with a fixed (maximum) duration.
-- The content is shorter at the end of a stream or when a 'Start'
-- 'FrameCtx' was sent.
newtype Segment c = MkSegment {_segmentContent :: c}
  deriving (NFData, Default, Arbitrary, Functor, Eq)

-- | An 'Iso' for the 'Segment' newtype.
segmentContent :: Iso (Segment c) (Segment c') c c'
segmentContent = iso _segmentContent MkSegment

instance (HasMedia c c') => HasMedia (Segment c) (Segment c') where
  type MediaFrom (Segment c) = MediaFrom c
  type MediaTo (Segment c') = MediaTo c'
  media = segmentContent . media


instance
  HasMediaBufferLens c c' =>
  HasMediaBufferLens (Segment c) (Segment c')
  where
   type MediaBufferElemFrom (Segment c) = MediaBufferElemFrom c
   type MediaBufferElemTo (Segment c') = MediaBufferElemTo c'
   mediaBufferLens = segmentContent . mediaBufferLens

instance (EachSample c c') => EachSample (Segment c) (Segment c') where
  type SamplesFrom (Segment c) = SamplesFrom c
  type SamplesTo (Segment c') = SamplesTo c'
  eachSample = segmentContent . eachSample

instance (EachChannel c c') => EachChannel (Segment c) (Segment c') where
  type ChannelsFrom (Segment c) = ChannelsFrom c
  type ChannelsTo (Segment c') = ChannelsTo c'
  eachChannel = segmentContent . eachChannel

instance (HasRate c) => HasRate (Segment c) where
  type GetRate (Segment c) = GetRate c
  type SetRate (Segment c) r' = Segment (SetRate c r')

instance
  (HasRate i, GetRate i ~ ri, SetRate i rj ~ j, KnownRate rj, CoerceRate i j ri rj) =>
  CoerceRate (Segment i) (Segment j) ri rj
  where
  coerceRate px (MkSegment !c) = MkSegment (coerceRate px c)

instance
  (HasDuration c, Show c) =>
  Show (Segment c)
  where
  showsPrec _d (MkSegment c) =
    showString "[| "
      . shows c
      . showString (printf " |%10s]" (show (getDuration c)))

instance
  HasDuration x =>
  HasDuration (Segment x)
  where
  getDuration (MkSegment x) = getDuration x

-- | Class of types that support splitting values into parts with a certain
--   duration.
class CanSegment a where
  -- | Try to split the packet into the a part which has the given
  -- duration and a rest. If it is not possible to split of the desired duration,
  --  e.g. because the input data is too short, return `Nothing`.
  splitAfterDuration :: NominalDiffTime -> a -> Maybe (a, a)
