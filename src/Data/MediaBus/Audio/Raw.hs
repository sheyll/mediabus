{-# LANGUAGE TemplateHaskell #-}

module Data.MediaBus.Audio.Raw
    ( S16(..)
    , s16Sample
    , IsAudioSample(..)
    , setAudioSampleRateTo8kHz
    , setAudioSampleRateTo16kHz
    , setAudioSampleRateTo48kHz
    , doubleAudioSampleRate
    ) where

import           Foreign.Storable
import           Data.MediaBus.Audio.Channels
import           Data.MediaBus.BlankMedia
import           Data.Int
import           Control.Lens
import           Test.QuickCheck
import           Data.Bits
import           Data.Typeable
import           Data.MediaBus.Ticks
import           GHC.TypeLits
import           Data.Function                ( on )
import           GHC.Generics                 ( Generic )
import           Control.DeepSeq

newtype S16 (rate :: Nat) = MkS16 { _s16Sample :: Int16 }
    deriving (Typeable, Storable, Num, Eq, Ord, Arbitrary, Generic)

instance NFData (S16 rate)

instance KnownNat r =>
         HasDuration (Proxy (S16 r)) where
    getDuration _ = 1 / fromInteger (natVal (Proxy :: Proxy r))
    getDurationTicks _ = convertTicks (MkTicks 1 :: Ticks r Int)

instance Show (S16 r) where
    show (MkS16 x) = show x

makeLenses ''S16

class (KnownNat (GetAudioSampleRate a), SetAudioSampleRate a (GetAudioSampleRate a) ~ a, Show a, Storable a, Eq a, Ord a, Arbitrary a) =>
      IsAudioSample a where
    type GetAudioSampleRate a :: Nat
    type SetAudioSampleRate a (b :: Nat)
    avgSamples :: a -> a -> a
    setAudioSampleRate :: KnownNat r => proxy r -> a -> SetAudioSampleRate a r

instance KnownNat r =>
         IsAudioSample (S16 r) where
    type SetAudioSampleRate (S16 r) x = S16 x
    type GetAudioSampleRate (S16 r) = r
    avgSamples (MkS16 !x) (MkS16 !y) =
        MkS16 $
            if abs x < 16382 &&
                abs y < 16382
            then (x + y) `unsafeShiftR` 1
            else (x `unsafeShiftR` 1) + (y `unsafeShiftR` 1)
    setAudioSampleRate _ (MkS16 !x) =
        MkS16 x

instance CanBeBlank (S16 r) where
    blank = MkS16 0

instance (IsAudioSample a) =>
         IsAudioSample (ChannelPair a) where
    type SetAudioSampleRate (ChannelPair a) x = ChannelPair (SetAudioSampleRate a x)
    type GetAudioSampleRate (ChannelPair a) = GetAudioSampleRate a
    avgSamples !x !y = MkChannelPair ((avgSamples `on` _leftSample) x y)
                                     ((avgSamples `on` _rightSample) x y)
    setAudioSampleRate !p (MkChannelPair !l !r) =
        MkChannelPair (setAudioSampleRate p l) (setAudioSampleRate p r)

setAudioSampleRateTo8kHz :: (IsAudioSample x, y ~ SetAudioSampleRate x 8000)
                         => x
                         -> y
setAudioSampleRateTo8kHz =
    setAudioSampleRate (Proxy :: Proxy 8000)

setAudioSampleRateTo16kHz :: (IsAudioSample x, y ~ SetAudioSampleRate x 16000)
                          => x
                          -> y
setAudioSampleRateTo16kHz =
    setAudioSampleRate (Proxy :: Proxy 16000)

setAudioSampleRateTo48kHz :: (IsAudioSample x, y ~ SetAudioSampleRate x 48000)
                          => x
                          -> y
setAudioSampleRateTo48kHz =
    setAudioSampleRate (Proxy :: Proxy 48000)

doubleAudioSampleRate :: forall a b.
                      (IsAudioSample a, b ~ SetAudioSampleRate a (GetAudioSampleRate a + GetAudioSampleRate a), KnownNat (GetAudioSampleRate a + GetAudioSampleRate a))
                      => a
                      -> b
doubleAudioSampleRate = setAudioSampleRate (Proxy :: Proxy (GetAudioSampleRate a + GetAudioSampleRate a))

instance HasChannelLayout (S16 r) where
    channelLayout _ = SingleChannel
