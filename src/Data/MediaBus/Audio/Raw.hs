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
import           Data.MediaBus.MediaData
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

-- | Family of audio sample types, indexed by the 'ChannelLayout' because the channel layout
-- determines the low level repesentation (the 'Storable' instance) of the
data family Audio (sampleDuration :: StaticTicks) (c :: ChannelLayout) t

-- | Types that can be used to store audio samples
class (CanBeBlank a, Storable a, NFData a, Eq a, Num a) =>
      IsAudioSample a where
    avgSamples :: a -> a -> a

-- | A new type wrapper to indicate that a type is a _raw_ i.e. not encoded or
-- compressed audio sample in the time domain.
newtype RawAudio t = MkRawAudio { _rawAudioSample :: t }
  deriving (Storable, Num, Eq, Ord, Arbitrary, Generic, IsAudioSample)

instance NFData RawAudio

instance CanBeBlank RawAudio where
    blank = 0

-- | An 'Iso' for the raw audio sample content
rawAudioSample :: Iso s t (RawAudio s) (RawAudio t)
rawAudioSample = iso MkRawAudio _rawAudioSample

-- | Newtype wrapper for _mono_ aka 'SingleChannel' audio.
newtype instance
        Audio d 'SingleChannel (RawAudio t) = MkSingleChannel{_singleChannelSample
                                                              ::  RawAudio t}
                                 deriving (Show, Eq, Ord, NFData, Storable, CanBeBlank, Num,
                                            Arbitrary)

-- | An 'Iso' for the single channel content
singleChannelSample :: Iso s t (Audio d 'SingleChannel s) (Audio d 'SingleChannel t)
singleChannelSample = iso MkSingleChannel _singleChannelSample

-- | Stereo audio samples.
data instance
     Audio d 'ChannelPair (RawAudio t) = MkChannelPair{_leftSample :: !t,
                                                       _rightSample :: !t}
                            deriving (Show, Eq, Ord, Generic)

-- | A simple lens for the left sample
leftSample :: Lens' (Audio d 'ChannelPair s) s
leftSample = lens _leftSample (\p l -> p { _leftSample = l })

-- | A simple lens for the right sample
rightSample :: Lens' (Audio d 'ChannelPair s) s
rightSample = lens _rightSample (\p r -> p { _rightSample = r })

instance NFData a =>
         NFData (Audio d 'ChannelPair  a)

instance Arbitrary a =>
         Arbitrary (Audio d 'ChannelPair  a) where
    arbitrary = MkChannelPair <$> arbitrary <*> arbitrary

instance Storable a =>
         Storable (Audio d 'ChannelPair  a) where
    sizeOf s = 2 * sizeOf s
    alignment = alignment
    peekByteOff ptr off = do
        l <- peekByteOff ptr off
        let rOffset = sizeOf l
        r <- peekByteOff ptr (rOffset + off)
        return (MkChannelPair l r)
    pokeByteOff ptr off (MkChannelPair l r) = do
        pokeByteOff ptr off l
        let rOffset = sizeOf l
        pokeByteOff ptr (off + rOffset) r

instance ( Storable (c t)
         , NFData (c t)
         , Eq (c t)
         , CanBeBlank (c t)
         , HasChannelLayout (c t)
         , KnownStaticTicks d
         , IsAudioSample t) =>
         IsMedia (Audio d c t)

type S16 (rate :: Nat) = Audio (1 :@ rate) SingleChannel RawAudio


instance =>
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
