-- | This module defines the 16-bit PCM audio sample type.
module Data.MediaBus.Media.Audio.Raw.Signed16bit
  ( S16(..)
  , s16Sample
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Bits
import Data.Default (Default)
import Data.Int
import Data.MediaBus.Media.Audio.Raw
import Data.MediaBus.Media.Blank
import Data.Typeable
import Data.Vector.Storable (Storable)
import Test.QuickCheck
import Text.Printf

-- | A value representing a signed PCM audio sample with a width of 16 bit.
newtype S16 = MkS16
  { _s16Sample :: Int16
  } deriving ( Eq
             , Ord
             , Arbitrary
             , Bits
             , NFData
             , Storable
             , Num
             , Default
             , Typeable
             )

-- | An 'Iso' from/to the sample value of an 'S16'
s16Sample :: Iso' S16 Int16
s16Sample = iso _s16Sample MkS16

instance Show S16 where
  showsPrec d (MkS16 !x) = showParen (d > 10) $ showString (printf "S16: %6d" x)

instance CanBeBlank S16 where
  blank = 0

instance IsPcmValue S16 where
  pcmAverage !x !y =
    if abs x < 16382 && abs y < 16382
      then (x + y) `unsafeShiftR` 1
      else (x `unsafeShiftR` 1) + (y `unsafeShiftR` 1)
