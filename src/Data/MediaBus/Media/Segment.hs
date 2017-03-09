-- | Media segments with a fixed duration

{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Media.Segment
    ( Segment(..)
    , segmentContent
    , CanSegment(..)
    ) where

import           Data.MediaBus.Basics.Ticks
import           Data.MediaBus.Media.Media
import           Data.MediaBus.Media.Channels
import           Data.MediaBus.Media.Samples
import           Control.Lens
import           Data.Default
import           Data.Proxy
import           Test.QuickCheck
import           Control.DeepSeq
import           Text.Printf

-- | A segment is some content with a fixed (type level) duration.
newtype Segment (duration :: StaticTicks) c = MkSegment { _segmentContent :: c }
    deriving (NFData, Default, Arbitrary, Functor, Eq)

-- | An 'Iso' for the 'Segment' newtype.
segmentContent :: Iso (Segment d c) (Segment d c') c c'
segmentContent = iso _segmentContent MkSegment

instance (HasMedia c c') => HasMedia (Segment d c)  (Segment d c') where
  type MediaFrom (Segment d c) = MediaFrom c
  type MediaTo (Segment d c') = MediaTo c'
  media = segmentContent . media

instance (EachSample c c') => EachSample (Segment d c)  (Segment d c') where
  type SamplesFrom (Segment d c) = SamplesFrom c
  type SamplesTo (Segment d c') = SamplesTo c'
  eachSample = segmentContent . eachSample

instance (EachChannel c c') => EachChannel (Segment d c)  (Segment d c') where
  type ChannelsFrom (Segment d c) = ChannelsFrom c
  type ChannelsTo (Segment d c') = ChannelsTo c'
  eachChannel = segmentContent . eachChannel

instance (HasRate c) => HasRate (Segment d c) where
  type GetRate (Segment d c) = GetRate c
  type SetRate (Segment d c) r' = Segment d (SetRate c r')

instance (HasRate i, GetRate i ~ ri, SetRate i rj ~ j, KnownRate rj, CoerceRate i j ri rj) =>
      CoerceRate (Segment d i) (Segment d j) ri rj
    where
  coerceRate px (MkSegment !c) = MkSegment (coerceRate px c)

instance (HasStaticDuration d, Show c) =>
         Show (Segment d c) where
    showsPrec _d (MkSegment c) =
          showString "[| "
          . shows c
          . showString (printf " |%10s]" (show (getStaticDuration (Proxy :: Proxy d))))

instance KnownStaticTicks d =>
         HasStaticDuration (Segment d x) where
    type SetStaticDuration (Segment d x) pt = Segment pt x
    type GetStaticDuration (Segment d x) = d

instance HasStaticDuration d =>
         HasDuration (Segment d x) where
    getDuration _ = getStaticDuration (Proxy :: Proxy d)

-- | Class of types that support splitting values into parts with a certain
--   duration.
class CanSegment a where
    -- | Try to split the packet into the a part which has the given
    -- duration and a rest. If it is not possible to split of the desired duration,
    --  e.g. because the input data is too short, return `Nothing`.
    splitAfterDuration :: (HasStaticDuration d)
                       => proxy d
                       -> a
                       -> Maybe (Segment d a, a)
