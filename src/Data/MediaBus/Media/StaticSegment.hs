{-# LANGUAGE UndecidableInstances #-}

-- | Media segments with a fixed duration
module Data.MediaBus.Media.StaticSegment
  ( StaticSegment (..),
    staticSegmentContent,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens (Iso, iso)
import Data.Default (Default)
import Data.MediaBus.Basics.Ticks
  ( CoerceRate (..),
    HasDuration (getDuration),
    HasRate (..),
    HasStaticDuration (..),
    KnownRate,
    KnownStaticTicks,
    StaticTicks,
  )
import Data.MediaBus.Media.Channels (EachChannel (..))
import Data.MediaBus.Media.Media (HasMedia (..))
import Data.MediaBus.Media.Samples (EachSample (..))
import Test.QuickCheck (Arbitrary)
import Text.Printf (printf)
import Data.MediaBus.Media.Buffer

-- | A segment is some content with a fixed (type level) duration.
-- The payload however, has a duration of less than the static duration.
--
-- The only case where the content is shorter
newtype StaticSegment (duration :: StaticTicks) c = MkStaticSegment {_staticSegmentContent :: c}
  deriving (NFData, Default, Arbitrary, Functor, Eq)

-- | An 'Iso' for the 'StaticSegment' newtype.
staticSegmentContent :: Iso (StaticSegment d c) (StaticSegment d c') c c'
staticSegmentContent = iso _staticSegmentContent MkStaticSegment

instance
  HasMediaBufferLens c c' =>
  HasMediaBufferLens (StaticSegment d c) (StaticSegment d c')
  where
   type MediaBufferElemFrom (StaticSegment d c) = MediaBufferElemFrom c
   type MediaBufferElemTo (StaticSegment d c') = MediaBufferElemTo c'
   mediaBufferLens = staticSegmentContent . mediaBufferLens

instance (HasMedia c c') => HasMedia (StaticSegment d c) (StaticSegment d c') where
  type MediaFrom (StaticSegment d c) = MediaFrom c
  type MediaTo (StaticSegment d c') = MediaTo c'
  media = staticSegmentContent . media

instance (EachSample c c') => EachSample (StaticSegment d c) (StaticSegment d c') where
  type SamplesFrom (StaticSegment d c) = SamplesFrom c
  type SamplesTo (StaticSegment d c') = SamplesTo c'
  eachSample = staticSegmentContent . eachSample

instance (EachChannel c c') => EachChannel (StaticSegment d c) (StaticSegment d c') where
  type ChannelsFrom (StaticSegment d c) = ChannelsFrom c
  type ChannelsTo (StaticSegment d c') = ChannelsTo c'
  eachChannel = staticSegmentContent . eachChannel

instance (HasRate c) => HasRate (StaticSegment d c) where
  type GetRate (StaticSegment d c) = GetRate c
  type SetRate (StaticSegment d c) r' = StaticSegment d (SetRate c r')

instance
  (HasRate i, GetRate i ~ ri, SetRate i rj ~ j, KnownRate rj, CoerceRate i j ri rj) =>
  CoerceRate (StaticSegment d i) (StaticSegment d j) ri rj
  where
  coerceRate px (MkStaticSegment !c) = MkStaticSegment (coerceRate px c)

instance
  (HasDuration c, Show c) =>
  Show (StaticSegment d c)
  where
  showsPrec _d (MkStaticSegment c) =
    showString "[| "
      . shows c
      . showString (printf " |%10s]" (show (getDuration c)))

instance
  KnownStaticTicks d =>
  HasStaticDuration (StaticSegment d x)
  where
  type SetStaticDuration (StaticSegment d x) pt = StaticSegment pt x
  type GetStaticDuration (StaticSegment d x) = d

instance
  HasDuration x =>
  HasDuration (StaticSegment d x)
  where
  getDuration (MkStaticSegment x) = getDuration x
