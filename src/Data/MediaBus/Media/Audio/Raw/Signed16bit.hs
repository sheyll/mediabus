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

makeLenses ''S16

instance Show S16 where
  show (MkS16 !x) = printf "S16: %6d" x

instance CanBeBlank S16 where
  blank = 0

instance IsPcmValue S16 where
  pcmAverage !x !y =
    if abs x < 16382 && abs y < 16382
      then (x + y) `unsafeShiftR` 1
      else (x `unsafeShiftR` 1) + (y `unsafeShiftR` 1)
