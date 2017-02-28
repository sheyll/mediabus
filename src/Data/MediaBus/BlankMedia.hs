module Data.MediaBus.BlankMedia
    ( CanGenerateBlankMedia(..)
    , CanBeBlank(..)
    ) where

import           Data.MediaBus.Ticks
import           Data.MediaBus.Segment
import           Data.Time.Clock
import           GHC.TypeLits
import           Control.Lens
import           Data.Proxy

class CanGenerateBlankMedia a where
    blankFor :: NominalDiffTime -> a
    blankFor dt = blankForTicks (nominalDiffTime # dt :: Ticks 1000000000000 Integer)
    blankForTicks :: (Integral i, KnownNat r) => Ticks r i -> a
    blankForTicks ticks = blankFor (from nominalDiffTime # ticks)

class CanBeBlank a where
    blank :: a

instance (HasStaticDuration d, CanGenerateBlankMedia a) =>
         CanBeBlank (Segment d a) where
    blank = MkSegment (blankFor (getStaticDuration (Proxy :: Proxy d)))
