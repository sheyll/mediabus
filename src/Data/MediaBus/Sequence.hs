module Data.MediaBus.Sequence
    ( SeqNum(..)
    , type SeqNum8
    , type SeqNum16
    , type SeqNum32
    , type SeqNum64
    , HasSeqNumT(..)
    , HasSeqNum(..)
    , fromSeqNum
    , synchronizeToSeqNum
    ) where

import           Test.QuickCheck            ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Monotone
import           Data.MediaBus.Series
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Text.Printf
import           GHC.Generics               ( Generic )
import           Control.DeepSeq
import           System.Random
import           Data.Word

class SetSeqNum t (GetSeqNum t) ~ t =>
      HasSeqNumT t where
    type GetSeqNum t
    type SetSeqNum t s

class HasSeqNumT t =>
      HasSeqNum t where
    seqNum :: Lens t (SetSeqNum t s) (GetSeqNum t) s

instance (HasSeqNumT a, HasSeqNumT b, GetSeqNum a ~ GetSeqNum b) =>
         HasSeqNumT (Series a b) where
    type GetSeqNum (Series a b) = GetSeqNum a
    type SetSeqNum (Series a b) t = Series (SetSeqNum a t) (SetSeqNum b t)

instance (HasSeqNum a, HasSeqNum b, GetSeqNum a ~ GetSeqNum b) =>
         HasSeqNum (Series a b) where
    seqNum f (Start !a) = Start <$> seqNum f a
    seqNum f (Next !b) = Next <$> seqNum f b

newtype SeqNum s = MkSeqNum { _fromSeqNum :: s }
    deriving (Num, Eq, Bounded, Enum, LocalOrd, Arbitrary, Default, Generic, Random)

type SeqNum8 = SeqNum Word8

type SeqNum16 = SeqNum Word16

type SeqNum32 = SeqNum Word32

type SeqNum64 = SeqNum Word64

instance NFData s =>
         NFData (SeqNum s)

makeLenses ''SeqNum

instance HasSeqNumT (SeqNum s) where
    type GetSeqNum (SeqNum s) = s
    type SetSeqNum (SeqNum s) s' = SeqNum s'

instance HasSeqNum (SeqNum s) where
    seqNum = fromSeqNum

instance Show s =>
         Show (SeqNum s) where
    show (MkSeqNum s) = printf "SEQNUM: %10s" (show s)

instance (Eq a, LocalOrd a) =>
         Ord (SeqNum a) where
    compare !x !y
        | x == y = EQ
        | x `succeeds` y = GT
        | otherwise = LT

deriving instance (Real a, Num a, Eq a, LocalOrd a) => Real
         (SeqNum a)

deriving instance (Integral a, Enum a, Real a, Eq a, LocalOrd a) =>
         Integral (SeqNum a)

synchronizeToSeqNum :: (HasSeqNum a, Monad m, Integral i)
                    => i
                    -> Conduit a m (SetSeqNum a i)
synchronizeToSeqNum startSeq =
    evalStateC startSeq (awaitForever yieldSeq)
  where
    yieldSeq !a = do
        !nextSeq <- get
        modify (+ 1)
        yield (a & seqNum .~ nextSeq)
