-- | This modules contains the 'SeqNum' newtype wrapper to indicate that a type
-- is a sequence number.
module Data.MediaBus.Basics.Sequence
  ( SeqNum (..),
    fromSeqNum,
    type SeqNum8,
    type SeqNum16,
    type SeqNum32,
    type SeqNum64,
    HasSeqNum (..),
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Default
import Data.MediaBus.Basics.Monotone
import Data.MediaBus.Basics.Series
import Data.Word
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck (Arbitrary (..))
import Text.Printf

-- | The newtype wrapper that indicates that something is a sequence number.
newtype SeqNum s
  = MkSeqNum
      { _fromSeqNum :: s
      }
  deriving
    ( Num,
      Eq,
      Bounded,
      Enum,
      LocalOrd,
      Arbitrary,
      Default,
      Generic,
      Random
    )

instance
  NFData s =>
  NFData (SeqNum s)

instance HasSeqNum (SeqNum s) where
  type GetSeqNum (SeqNum s) = s
  type SetSeqNum (SeqNum s) s' = SeqNum s'
  seqNum = fromSeqNum

instance
  Show s =>
  Show (SeqNum s)
  where
  showsPrec d (MkSeqNum s) =
    showParen (d > 10) (printf "sn: %10s" . showsPrec 11 s)

instance
  (Eq a, LocalOrd a) =>
  Ord (SeqNum a)
  where
  compare !x !y
    | x == y = EQ
    | x `succeeds` y = GT
    | otherwise = LT

deriving instance
  (Real a, Num a, Eq a, LocalOrd a) => Real (SeqNum a)

deriving instance
  (Integral a, Enum a, Real a, Eq a, LocalOrd a) =>
  Integral (SeqNum a)

-- | An 'Iso' between a 'SeqNum' and its' value.
fromSeqNum :: Iso (SeqNum a) (SeqNum b) a b
fromSeqNum = iso _fromSeqNum MkSeqNum

-- * Type aliases

-- | A 'Word8' based sequence number.
type SeqNum8 = SeqNum Word8

-- | A 'Word16' based sequence number.
type SeqNum16 = SeqNum Word16

-- | A 'Word32' based sequence number.
type SeqNum32 = SeqNum Word32

-- | A 'Word64' based sequence number.
type SeqNum64 = SeqNum Word64

-- * Lens type classes

-- | A class for types providing a lens to a sequence number.
class
  SetSeqNum t (GetSeqNum t) ~ t =>
  HasSeqNum t where
  type GetSeqNum t
  type SetSeqNum t s

  -- | A lens for the sequence number contained in @t@
  seqNum :: Lens t (SetSeqNum t s) (GetSeqNum t) s

instance
  (HasSeqNum a, HasSeqNum b, GetSeqNum a ~ GetSeqNum b) =>
  HasSeqNum (Series a b)
  where
  type GetSeqNum (Series a b) = GetSeqNum a
  type SetSeqNum (Series a b) t = Series (SetSeqNum a t) (SetSeqNum b t)
  seqNum f (Start !a) = Start <$> seqNum f a
  seqNum f (Next !b) = Next <$> seqNum f b
