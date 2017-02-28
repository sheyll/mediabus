module Data.MediaBus.Ticks
    ( HasDuration(..)
    , HasTimestampT(..)
    , HasTimestamp(..)
    , Ticks(..)
    , type Ticks32
    , type Ticks64
    , type Ticks32At8000
    , type Ticks32At16000
    , type Ticks32At48000
    , type Ticks64At8000
    , type Ticks64At16000
    , type Ticks64At48000
    , mkTicks
    , at8kHzU32
    , at16kHzU32
    , at48kHzU32
    , at16kHzU64
    , at48kHzU64
    , nominalDiffTime
    , convertTicks
    , deriveFrameTimestamp
    , StaticTicks(..)
    , KnownStaticTicks(..)
    , Rate(..)
    , type (:@)
    , type StaticTicksRate
    , type StaticTicksTicks
    , HasStaticDuration(..)
    , toStaticDurationProxy
    , getStaticDuration
    , getStaticTicks
    , getStaticRate
    , ticksFromStaticDuration
    , rateFromStaticDuration
    ) where

import           Conduit
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Data.MediaBus.Monotone
import           Data.MediaBus.Series
import           Data.Proxy
import           Data.Time.Clock
import           Data.Word
import           GHC.TypeLits
import           Test.QuickCheck
import           GHC.Generics               ( Generic )
import           Control.DeepSeq
import           System.Random
import Text.Printf

newtype Ticks (rate :: Nat) w = MkTicks { _ticks :: w }
    deriving (Eq, Real, Integral, Enum, LocalOrd, Num, Arbitrary, Default, Generic, Random)

type Ticks32 r = Ticks r Word32
type Ticks64 r = Ticks r Word64

type Ticks32At8000 = Ticks32 8000
type Ticks32At16000 = Ticks32 16000
type Ticks32At48000 = Ticks32 48000

type Ticks64At8000 = Ticks64 8000
type Ticks64At16000 = Ticks64 16000
type Ticks64At48000 = Ticks64 48000

instance NFData w =>
         NFData (Ticks rate w)

mkTicks :: forall proxy rate baseType.
        proxy '(rate, baseType)
        -> baseType
        -> Ticks rate baseType
mkTicks _ = MkTicks

at8kHzU32 :: Proxy '(8000, Word32)
at8kHzU32 = Proxy

at16kHzU32 :: Proxy '(16000, Word32)
at16kHzU32 = Proxy

at48kHzU32 :: Proxy '(48000, Word32)
at48kHzU32 = Proxy

at16kHzU64 :: Proxy '(16000, Word64)
at16kHzU64 = Proxy

at48kHzU64 :: Proxy '(48000, Word64)
at48kHzU64 = Proxy

convertTicks :: (Integral w, Integral w', KnownNat r, KnownNat r')
             => Ticks r w
             -> Ticks r' w'
convertTicks = view (from nominalDiffTime) . view nominalDiffTime

nominalDiffTime :: forall r w.
                (Integral w, KnownNat r)
                => Iso' (Ticks r w) NominalDiffTime
nominalDiffTime = iso (toNDT . _ticks) (MkTicks . fromNDT)
  where
    toNDT = (/ rate) . fromIntegral
    fromNDT = round . (* rate)
    rate = fromInteger $ natVal (Proxy :: Proxy r)

instance (KnownNat r, Integral w, Show w) =>
         Show (Ticks r w) where
    show tix@(MkTicks x) =
      printf "%10s (%10d @ %10d Hz)"
             (show (view nominalDiffTime tix))
             (toInteger x)
             (natVal (Proxy :: Proxy r))

instance (Eq w, LocalOrd w) =>
         Ord (Ticks rate w) where
    (<=) = flip succeeds

-- | Types with a duration (e.g. audio samples).
class HasDuration a where
    getDuration :: a -> NominalDiffTime
    getDuration !x = from nominalDiffTime #
        (getDurationTicks x :: Ticks 1000000000000 Integer)
    getDurationTicks :: (Integral i, KnownNat r) => a -> Ticks r i
    getDurationTicks !x = nominalDiffTime # getDuration x

instance HasDuration a => HasDuration (Maybe a) where
  getDuration Nothing = 0
  getDuration (Just !a) = getDuration a
  getDurationTicks Nothing = 0
  getDurationTicks (Just !a) = getDurationTicks a

-- TODO rename *Timestamp to *Tick
class SetTimestamp t (GetTimestamp t) ~ t =>
      HasTimestampT t where
    type GetTimestamp t
    type SetTimestamp t s

class HasTimestampT t -- TODO inline HasTimestampT again
       =>
      HasTimestamp t where
    timestamp :: Lens t (SetTimestamp t s) (GetTimestamp t) s
    timestamp' :: Lens' t (GetTimestamp t)
    timestamp' = timestamp

instance (HasTimestampT a, HasTimestampT b, GetTimestamp a ~ GetTimestamp b) =>
         HasTimestampT (Series a b) where
    type GetTimestamp (Series a b) = GetTimestamp a
    type SetTimestamp (Series a b) t = Series (SetTimestamp a t) (SetTimestamp b t)

instance (HasTimestamp a, HasTimestamp b, GetTimestamp a ~ GetTimestamp b) =>
         HasTimestamp (Series a b) where
    timestamp f (Start a) = Start <$> timestamp f a
    timestamp f (Next b) = Next <$> timestamp f b

-- * Media Data Synchronization
deriveFrameTimestamp :: (Monad m, KnownNat r, Integral t, HasDuration a, HasTimestamp a)
                     => Ticks r t
                     -> Conduit a m (SetTimestamp a (Ticks r t))
deriveFrameTimestamp t0 =
    evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync sb = do
        t <- get
        modify (+ (nominalDiffTime # getDuration sb))
        yield (sb & timestamp .~ t)

data StaticTicks where
  MkStaticTicks :: Nat -> Rate -> StaticTicks

data Rate = MkRate Nat

type ticks :@ rate = 'MkStaticTicks ticks ('MkRate rate)

instance (KnownNat r, KnownNat t) =>
         HasStaticDuration (t :@ r) where
    type SetStaticDuration (t :@ r) (t' :@ r') =  t' :@ r'
    type GetStaticDuration (t :@ r) = t :@ r

type family StaticTicksRate (s :: StaticTicks) :: Nat where
        StaticTicksRate (t :@ r) = r

type family StaticTicksTicks (s :: StaticTicks) :: Nat where
        StaticTicksTicks (t :@ r) = t

class KnownStaticTicks (s :: StaticTicks) where
    staticTicksVal :: KnownNat r => proxy s -> Ticks r Integer

instance (KnownNat d, KnownNat r) => KnownStaticTicks ('MkStaticTicks d ('MkRate r)) where
    staticTicksVal _ =
      convertTicks (MkTicks (natVal (Proxy :: Proxy d)) :: Ticks r Integer)

class (KnownStaticTicks (GetStaticDuration s), SetStaticDuration s (GetStaticDuration s) ~ s) =>
      HasStaticDuration (s :: k) where
    type SetStaticDuration s (pt :: StaticTicks) :: k'
    type GetStaticDuration s :: StaticTicks

toStaticDurationProxy :: (HasStaticDuration s)
                      => proxy s
                      -> Proxy (GetStaticDuration s)
toStaticDurationProxy _ = Proxy

getStaticDuration :: forall proxy s.
                  HasStaticDuration s
                  => proxy s
                  -> NominalDiffTime
getStaticDuration px = from nominalDiffTime #
    (staticTicksVal (toStaticDurationProxy px) :: Ticks 1000000000000 Integer)

getStaticTicks :: forall proxy s r t i.
               (KnownNat r, KnownNat t, HasStaticDuration s, GetStaticDuration s ~ (t :@ r), Integral i)
               => proxy s
               -> Ticks r i
getStaticTicks px = ticksFromStaticDuration (toStaticDurationProxy px)

getStaticRate :: forall proxy s r t.
              (KnownNat r, KnownNat t, HasStaticDuration s, GetStaticDuration s ~ (t :@ r))
              => proxy s
              -> Integer
getStaticRate px = rateFromStaticDuration (toStaticDurationProxy px)

ticksFromStaticDuration :: forall proxy rate ticks i.
                (KnownNat rate, KnownNat ticks, Integral i)
                => proxy (ticks :@ rate)
                -> Ticks rate i
ticksFromStaticDuration _ = MkTicks (fromIntegral (natVal (Proxy :: Proxy ticks)))

rateFromStaticDuration :: forall proxy rate ticks.
               (KnownNat rate, KnownNat ticks)
               => proxy (ticks :@ rate)
               -> Integer
rateFromStaticDuration _ = fromIntegral (natVal (Proxy :: Proxy rate))
