-- | Type class for media content that has /blank/ values, that represent a
-- neutral media content such as silence or a black image.
module Data.MediaBus.Media.Blank
  ( CanGenerateBlankMedia (..),
    CanBeBlank (..),
  )
where

import Control.Lens (from, (#))
import Data.MediaBus.Basics.Ticks
  ( CanBeTicks,
    HasStaticDuration,
    PicoSeconds,
    Ticks,
    getStaticDuration,
    nominalDiffTime,
  )
import Data.Proxy (Proxy (..))
import Data.Time.Clock (NominalDiffTime)
import Data.MediaBus.Media.StaticSegment

-- | Types that can have /blank/ values.
class CanBeBlank a where
  -- | Generate the value that represents neutral media content.
  blank :: a

-- | Types that have a dynamic duration, for example a audio sample buffers, can
-- implement this type class to provide methods for generating blank media
-- content (e.g. silence) for a certain duration.
class CanGenerateBlankMedia a where
  -- | Generate the value that represents neutral media content, and has at
  -- least the given duration.
  blankFor :: NominalDiffTime -> a
  blankFor dt = blankForTicks (nominalDiffTime # dt :: PicoSeconds)

  -- | Generate the value that represents neutral media content, and has at
  -- least the given duration given as 'Ticks'
  blankForTicks :: CanBeTicks r i => Ticks r i -> a
  blankForTicks ticks = blankFor (from nominalDiffTime # ticks)

instance
  (HasStaticDuration d, CanGenerateBlankMedia a) =>
  CanBeBlank (StaticSegment d a)
  where
  blank = MkStaticSegment (blankFor (getStaticDuration (Proxy :: Proxy d)))

