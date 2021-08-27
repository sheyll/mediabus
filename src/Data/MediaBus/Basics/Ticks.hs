-- | This module contains the 'Ticks' time unit as well as the corresponding
-- known-at-compile-time 'StaticTicks' time unit. The time stamps are given as
-- quotient of a 'Rate' that indicates the number of 'Ticks' per second.
module Data.MediaBus.Basics.Ticks
  ( Rate (..),
    type Hz,
    type OnePerPicoSecond,
    KnownRate (..),
    HasRate (..),
    CoerceRate (..),
    getRate,
    getRateProxy,
    RateProxy (..),
    type PeriodDuration,
    getPeriodDuration,
    coerceRateTo8kHz,
    coerceRateTo16kHz,
    coerceRateTo48kHz,
    coerceToDoubleRate,
    Ticks (..),
    CanBeTicks,
    type PicoSeconds,
    type Ticks32,
    mkTicks32,
    type Ticks64,
    mkTicks64,
    type Ticks32At8000,
    mkTicks32At8000,
    type Ticks32At16000,
    mkTicks32At16000,
    type Ticks32At48000,
    mkTicks32At48000,
    type Ticks64At8000,
    mkTicks64At8000,
    type Ticks64At16000,
    mkTicks64At16000,
    type Ticks64At48000,
    mkTicks64At48000,
    nominalDiffTime,
    convertTicks,
    StaticTicks (..),
    KnownStaticTicks (..),
    type StaticTicksRate,
    type StaticTicksTicks,
    HasDuration (..),
    HasTimestamp (..),
    setTimestampFromDurations,
    removeTimestamp,
    HasStaticDuration (..),
    getStaticDurationTicks,
    getStaticDuration,
    toStaticDurationProxy,
    ticksFromStaticDuration,
    (:/),
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Default
import Data.Kind
import Data.MediaBus.Basics.Monotone
import Data.MediaBus.Basics.Series
import Data.Proxy
import Data.Time.Clock
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import System.Random
import Test.QuickCheck
import Text.Printf

-- * Time data types

-- ** A basic Time Unit

-- | The known at ompile time, time unit in units per second.
newtype Rate
  = Hertz Nat

-- | A more beautiful operator for 'Hertz'
type Hz r = 'Hertz r

-- | Analogous to 'KnownNat' this (kind-)class is for 'StaticTicks' with a runtime
-- 'Ticks' value.
class KnownRate (s :: Rate) where
  -- | Return the runtime rate value in Hertz
  rateVal :: proxy s -> Integer

  -- | Return the compile time rate value in Hertz
  type RateVal s :: Nat

instance
  (KnownNat r) =>
  KnownRate ('Hertz r)
  where
  rateVal _ = natVal (Proxy :: Proxy r)
  type RateVal ('Hertz r) = r

-- | A proxy type for 'Rate's useful to prevent orphan instances, is seen in
-- the 'Show' instance for 'RateProxy'. If the instance were defined as
-- @instance KnownRate r => Show (proxy r) where ... @ it would be an orphan
-- instance.
data RateProxy :: Rate -> Type where
  MkRateProxy :: RateProxy rate
  ConvertRateProxy :: proxy rate -> RateProxy rate

instance
  KnownRate r =>
  Show (RateProxy r)
  where
  showsPrec p _ =
    showParen (p > 10) (shows (rateVal (Proxy :: Proxy r)) . showString " Hz")

-- | Return the 'StaticTicks' representing the shortest representable duration
-- of something sampled at a 'Rate'
type PeriodDuration i = 1 :/ GetRate i

-- | The maximum representable frequency is @10e12 1/s@ which corresponds to
--   the resolution of 'NominalDiffTime', i.e. 1 pico second.
type OnePerPicoSecond = Hz 1000000000000

-- ** Types that have a /known/ 'Rate'

-- | Types with a known 'Rate', e.g. audio media has a sample rate.
class
  (KnownRate (GetRate i), SetRate i (GetRate i) ~ i) =>
  HasRate i where
  -- | Set the static sample rate of the media
  type SetRate i (r :: Rate)

  -- | The static sample rate of the media
  type GetRate i :: Rate

-- | Types which contain a rate, but are agnostic of it. The counter example would be
-- if the rate was a type index of a data family.
class
  (HasRate i, GetRate i ~ ri, SetRate i rj ~ j, KnownRate rj) =>
  CoerceRate i j ri rj where
  -- | Change the static sample rate, without e.g. resampling
  coerceRate :: proxy rj -> i -> SetRate i rj

-- | Return the 'Rate' as an 'Integer' from a proxy for an instance of 'HasRate'
getRate ::
  forall i proxy.
  HasRate i =>
  proxy i ->
  Integer
getRate = rateVal . getRateProxy

-- | Return a 'Proxy' for the 'GetRate' from a proxy for an instance of 'HasRate'
getRateProxy ::
  HasRate i =>
  proxy i ->
  RateProxy (GetRate i)
getRateProxy _ = MkRateProxy

-- | Return the reciprocal of the sample rate, i.e. the duration that one sample spans
getPeriodDuration ::
  forall i proxy.
  HasRate i =>
  proxy i ->
  NominalDiffTime
getPeriodDuration _ = 1 / fromInteger (rateVal (Proxy :: Proxy (GetRate i)))

-- | Utility around 'coerceRate' to set the sample rate to 8000 Hz.
coerceRateTo8kHz ::
  CoerceRate x y rx (Hz 8000) =>
  x ->
  y
coerceRateTo8kHz = coerceRate (Proxy :: Proxy (Hz 8000))

-- | Utility around 'coerceRate' to set the sample rate to 16000 Hz.
coerceRateTo16kHz ::
  CoerceRate x y rx (Hz 16000) =>
  x ->
  y
coerceRateTo16kHz = coerceRate (Proxy :: Proxy (Hz 16000))

-- | Utility around 'coerceRate' to set the sample rate to 48000 Hz.
coerceRateTo48kHz ::
  CoerceRate x y rx (Hz 48000) =>
  x ->
  y
coerceRateTo48kHz = coerceRate (Proxy :: Proxy (Hz 48000))

-- | Utility around 'coerceRate' to double the sample rate.
coerceToDoubleRate ::
  forall r s x y.
  ( CoerceRate x y r (Hz (s + s)),
    KnownRate r,
    RateVal r ~ s,
    KnownNat (s + s)
  ) =>
  x ->
  y
coerceToDoubleRate = coerceRate (Proxy :: Proxy (Hz (s + s)))

-- ** Arbitrary resolution (aka rate) integral time

-- | An integral time unit such that (time_in_seconds = _ticks * 1/rate)
newtype Ticks (rate :: Rate) w
  = MkTicks
      { _ticks :: w
      }
  deriving
    ( Eq,
      Real,
      Integral,
      Enum,
      LocalOrd,
      Num,
      Arbitrary,
      Default,
      Generic,
      Random
    )

-- | The constraint on the type parameters of 'Ticks
type CanBeTicks (r :: Rate) w = (KnownRate r, Integral w)

-- | The highest resolution 'Ticks' possible, such that it can still be
--   converted to 'NominalDiffTime'
type PicoSeconds = Ticks OnePerPicoSecond Integer

-- ** Smart constructors for 'Ticks'

-- | Alias for 'Ticks' based on a 'Word32' tick count.
type Ticks32 r = Ticks r Word32

-- | Create a 'Ticks32' from a 'Rate' given via a proxy and the number of ticks.
mkTicks32 ::
  KnownRate r =>
  proxy r ->
  Word32 ->
  Ticks32 r
mkTicks32 _ = MkTicks

-- | Alias for 'Ticks' based on a 'Word64' tick count.
type Ticks64 r = Ticks r Word64

-- | Create a 'Ticks64' from a 'Rate' given via a proxy and the number of ticks.
mkTicks64 ::
  KnownRate r =>
  proxy r ->
  Word64 ->
  Ticks64 r
mkTicks64 _ = MkTicks

-- | Alias for 'Ticks' based on a 'Word32' tick count with a rate of 8000 ticks per second.
type Ticks32At8000 = Ticks32 (Hz 8000)

-- | Create a 'Ticks32At8000' from a tick count.
mkTicks32At8000 :: Word32 -> Ticks32At8000
mkTicks32At8000 = MkTicks

-- | Alias for 'Ticks' based on a 'Word32' tick count with a rate of 16000 ticks per second.
type Ticks32At16000 = Ticks32 (Hz 16000)

-- | Create a 'Ticks32At16000 from a tick count.
mkTicks32At16000 :: Word32 -> Ticks32At16000
mkTicks32At16000 = MkTicks

-- | Alias for 'Ticks' based on a 'Word32' tick count with a rate of 48000 ticks per second.
type Ticks32At48000 = Ticks32 (Hz 48000)

-- | Create a 'Ticks32At48000 from a tick count.
mkTicks32At48000 :: Word32 -> Ticks32At48000
mkTicks32At48000 = MkTicks

-- | Alias for 'Ticks' based on a 'Word64' tick count with a rate of 8000 ticks per second.
type Ticks64At8000 = Ticks64 (Hz 8000)

-- | Create a 'Ticks64At8000 from a tick count.
mkTicks64At8000 :: Word64 -> Ticks64At8000
mkTicks64At8000 = MkTicks

-- | Alias for 'Ticks' based on a 'Word64' tick count with a rate of 16000 ticks per second.
type Ticks64At16000 = Ticks64 (Hz 16000)

-- | Create a 'Ticks64At16000 from a tick count.
mkTicks64At16000 :: Word64 -> Ticks64At16000
mkTicks64At16000 = MkTicks

-- | Alias for 'Ticks' based on a 'Word64' tick count with a rate of 48000 ticks per second.
type Ticks64At48000 = Ticks64 (Hz 48000)

-- | Create a 'Ticks64At48000 from a tick count.
mkTicks64At48000 :: Word64 -> Ticks64At48000
mkTicks64At48000 = MkTicks

instance
  NFData w =>
  NFData (Ticks rate w)

-- | Transform a 'Tick' value to another 'Tick' value.
convertTicks ::
  (CanBeTicks r w, CanBeTicks r' w') =>
  Ticks r w ->
  Ticks r' w'
convertTicks = view (from nominalDiffTime) . view nominalDiffTime

-- | A function (an 'Iso') that converts back-and-forth between 'Tick's and
--  'NominalDiffTime's
nominalDiffTime ::
  forall r w.
  (CanBeTicks r w) =>
  Iso' (Ticks r w) NominalDiffTime
nominalDiffTime = iso (toNDT . _ticks) (MkTicks . fromNDT)
  where
    toNDT = (/ rate) . fromIntegral
    fromNDT = round . (* rate)
    rate = fromInteger $ rateVal (Proxy :: Proxy r)

instance
  (CanBeTicks r w, Show w) =>
  Show (Ticks r w)
  where
  showsPrec d (MkTicks x) =
    showParen (d > 7)
      $ showString
      $ printf
        "%8d / %dHz"
        (toInteger x)
        (rateVal (Proxy :: Proxy r))

instance
  (Eq w, LocalOrd w) =>
  Ord (Ticks rate w)
  where
  (<=) = flip succeeds

-- ** Compile-Time Known Time values

-- | Time unit for durations known at compile time.
data StaticTicks where
  (:/:) :: Nat -> Rate -> StaticTicks

-- | Convenient wrapper around 'MkStaticTicks' and
--   'MkRate' to create a promoted 'StaticTicks'.
type ticks :/ rate = ticks ':/: rate

-- | Return the 'Rate' value of a promoted 'StaticTicks'.
type family StaticTicksRate (s :: StaticTicks) :: Rate where
  StaticTicksRate (t :/ r) = r

-- | Return the ticks value of a promoted 'StaticTicks'.
type family StaticTicksTicks (s :: StaticTicks) :: Nat where
  StaticTicksTicks (t :/ r) = t

-- | Analog to 'KnownNat' this (kind-)class is for 'StaticTicks' with a runtime
-- 'Ticks' value.
class KnownStaticTicks (s :: StaticTicks) where
  staticTicksVal ::
    KnownRate r =>
    proxy s ->
    Ticks r Integer

instance
  (KnownNat d, KnownRate r) =>
  KnownStaticTicks (d :/ r)
  where
  staticTicksVal _ =
    convertTicks (MkTicks (natVal (Proxy :: Proxy d)) :: Ticks r Integer)

-- * Types with a duration

-- ** Runtime duration values

-- | Types with a duration (e.g. audio samples).
class HasDuration a where
  getDuration :: a -> NominalDiffTime
  getDuration !x = from nominalDiffTime # (getDurationTicks x :: PicoSeconds)
  getDurationTicks ::
    (CanBeTicks r i) =>
    a ->
    Ticks r i
  getDurationTicks !x = nominalDiffTime # getDuration x

instance (CanBeTicks r i) => HasDuration (Ticks r i) where
  getDuration = view nominalDiffTime

instance HasDuration a => HasDuration [a] where
  getDurationTicks = sum . map getDurationTicks
  getDuration = sum . map getDuration

instance
  HasDuration a =>
  HasDuration (Maybe a)
  where
  getDuration Nothing = 0
  getDuration (Just !a) = getDuration a
  getDurationTicks Nothing = 0
  getDurationTicks (Just !a) = getDurationTicks a

-- | Types that contain a 'Timestamp'
class
  SetTimestamp t (GetTimestamp t) ~ t =>
  HasTimestamp t where
  type GetTimestamp t
  type SetTimestamp t s
  timestamp :: Lens t (SetTimestamp t s) (GetTimestamp t) s
  timestamp' :: Lens' t (GetTimestamp t)
  timestamp' = timestamp

-- | Calculate and set a timestamp.
--
-- The timestamp of each element is calculated from the sum of the durations of
-- the previous elements stored and the start time stamp @t0@.
--
-- The input elements must be instances of 'HasTimestamp' but with the important
-- condition, that the input timestamp is always /unit/ i.e. @()@.
-- This prevents /meaningful/ timestamps from being overwritten.
--
-- Use 'removeTimestamp' to explicitly remove a timestamp.
setTimestampFromDurations ::
  forall r t a.
  ( CanBeTicks r t,
    HasDuration a,
    HasTimestamp a,
    GetTimestamp a ~ ()
  ) =>
  a ->
  Ticks r t ->
  (SetTimestamp a (Ticks r t), Ticks r t)
setTimestampFromDurations !sb !t =
  (sb & timestamp .~ t, t + (nominalDiffTime # getDuration sb))

-- | Explicitly remove a timestamp, by setting the timestamp to @()@.
removeTimestamp :: (HasTimestamp a) => a -> SetTimestamp a ()
removeTimestamp = timestamp .~ ()

-- ** Known at compile time durations

-- *** Static ticks TODO rename static -> known

-- | Types that have a duration known at compoile time.
class
  ( KnownStaticTicks (GetStaticDuration s),
    SetStaticDuration s (GetStaticDuration s) ~ s
  ) =>
  HasStaticDuration (s :: k) where
  type SetStaticDuration s (pt :: StaticTicks) :: k
  type SetStaticDuration s (pt :: StaticTicks) = s
  type GetStaticDuration s :: StaticTicks

instance
  (KnownRate r, KnownNat t) =>
  HasStaticDuration (t :/ r)
  where
  type SetStaticDuration (t :/ r) (t' :/ r') = t' :/ r'
  type GetStaticDuration (t :/ r) = t :/ r

-- | Create a 'Proxy' for the 'StaticTicks' type associated with 's', this is
-- basically the analogon to the 'getDuration' method - just for types with a
-- duration known at compile time.
toStaticDurationProxy ::
  (HasStaticDuration s) =>
  proxy s ->
  Proxy (GetStaticDuration s)
toStaticDurationProxy _ = Proxy

-- | Convert the 'StaticDuration' that some type has to the number of seconds.
getStaticDuration ::
  forall proxy s.
  HasStaticDuration s =>
  proxy s ->
  NominalDiffTime
getStaticDuration px =
  from nominalDiffTime
    # (staticTicksVal (toStaticDurationProxy px) :: PicoSeconds)

-- | Convert the 'StaticDuration' that some type has to any 'Ticks'.
getStaticDurationTicks ::
  forall proxy s r t i.
  ( CanBeTicks r i,
    KnownNat t,
    HasStaticDuration s,
    GetStaticDuration s ~ (t :/ r)
  ) =>
  proxy s ->
  Ticks r i
getStaticDurationTicks px = ticksFromStaticDuration (toStaticDurationProxy px)

ticksFromStaticDuration ::
  forall proxy rate ticks i.
  (CanBeTicks rate i, KnownNat ticks) =>
  proxy (ticks :/ rate) ->
  Ticks rate i
ticksFromStaticDuration _ =
  MkTicks (fromIntegral (natVal (Proxy :: Proxy ticks)))

instance
  (HasTimestamp a, HasTimestamp b, GetTimestamp a ~ GetTimestamp b) =>
  HasTimestamp (Series a b)
  where
  type GetTimestamp (Series a b) = GetTimestamp a
  type SetTimestamp (Series a b) t = Series (SetTimestamp a t) (SetTimestamp b t)
  timestamp f (Start a) = Start <$> timestamp f a
  timestamp f (Next b) = Next <$> timestamp f b
