module Data.MediaBus.Media.Blank
    ( CanGenerateBlankMedia(..)
    , CanBeBlank(..)
    ) where

import           Data.MediaBus.Ticks
import           Data.MediaBus.Media.Segment
import           Data.Time.Clock
import           Control.Lens
import           Data.Proxy

class CanGenerateBlankMedia a where
    blankFor :: NominalDiffTime -> a
    blankFor dt = blankForTicks (nominalDiffTime # dt :: PicoSeconds)
    blankForTicks :: CanBeTicks r i => Ticks r i -> a
    blankForTicks ticks = blankFor (from nominalDiffTime # ticks)

class CanBeBlank a where
    blank :: a

instance (HasStaticDuration d, CanGenerateBlankMedia a) =>
         CanBeBlank (Segment d a) where
    blank = MkSegment (blankFor (getStaticDuration (Proxy :: Proxy d)))
