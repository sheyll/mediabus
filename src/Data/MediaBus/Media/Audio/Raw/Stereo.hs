-- | Stereo PCM audio
module Data.MediaBus.Media.Audio.Raw.Stereo
  ( Stereo()
  ) where

import Control.DeepSeq
import Data.Default
import Data.MediaBus.Media.Audio.Raw
import Data.MediaBus.Media.Blank
import Data.MediaBus.Media.Channels
import Data.Typeable
import Foreign.Storable
import GHC.Generics (Generic)
import Test.QuickCheck

-- | The channel layout indicator type for **stereo** audio
data Stereo

instance KnownChannelLayout Stereo where
  numberOfChannels _ = 2

data instance  Pcm Stereo t = MkStereo{_leftSample :: !t,
                                       _rightSample :: !t}
                            deriving (Eq, Ord, Generic, Typeable)

instance NFData t =>
         NFData (Pcm Stereo t)

instance Arbitrary t =>
         Arbitrary (Pcm Stereo t) where
  arbitrary = MkStereo <$> arbitrary <*> arbitrary

instance EachChannel (Pcm Stereo a) (Pcm Stereo b) where
  type ChannelsFrom (Pcm Stereo a) = a
  type ChannelsTo (Pcm Stereo b) = b
  eachChannel f (MkStereo !l !r) = MkStereo <$> f l <*> f r

instance IsPcmValue a =>
         IsPcmValue (Pcm Stereo a) where
  pcmAverage (MkStereo !l0 !r0) (MkStereo !l1 !r1) =
    MkStereo (pcmAverage l0 l1) (pcmAverage r0 r1)

instance CanBeBlank a =>
         CanBeBlank (Pcm Stereo a) where
  blank = MkStereo blank blank

instance Default a =>
         Default (Pcm Stereo a) where
  def = MkStereo def def

instance Storable s =>
         Storable (Pcm Stereo s) where
  sizeOf s = 2 * sizeOf s
  alignment = alignment
  peekByteOff ptr off = do
    l <- peekByteOff ptr off
    let rOffset = sizeOf l
    r <- peekByteOff ptr (rOffset + off)
    return (MkStereo l r)
  pokeByteOff ptr off (MkStereo l r) = do
    pokeByteOff ptr off l
    let rOffset = sizeOf l
    pokeByteOff ptr (off + rOffset) r
