-- | Stereo PCM audio
module Data.MediaBus.Media.Audio.Raw.Stereo
  ( Stereo (),
  )
where

import Control.DeepSeq (NFData)
import Data.Default (Default (..))
import Data.MediaBus.Media.Audio.Raw (IsPcmValue (..), Pcm)
import Data.MediaBus.Media.Blank (CanBeBlank (..))
import Data.MediaBus.Media.Channels
  ( EachChannel (..),
    KnownChannelLayout (..),
  )
import Data.Typeable (Typeable)
import Foreign.Storable
  ( Storable (alignment, peekByteOff, pokeByteOff, sizeOf),
  )
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary))
import Foreign.Ptr

-- | The channel layout indicator type for **stereo** audio
data Stereo

instance KnownChannelLayout Stereo where
  numberOfChannels _ = 2

data instance Pcm Stereo t = MkStereo
  { _leftSample :: !t,
    _rightSample :: !t
  }
  deriving (Eq, Ord, Generic, Typeable)

instance
  NFData t =>
  NFData (Pcm Stereo t)

instance
  Arbitrary t =>
  Arbitrary (Pcm Stereo t)
  where
  arbitrary = MkStereo <$> arbitrary <*> arbitrary

instance EachChannel (Pcm Stereo a) (Pcm Stereo b) where
  type ChannelsFrom (Pcm Stereo a) = a
  type ChannelsTo (Pcm Stereo b) = b
  eachChannel f (MkStereo !l !r) = MkStereo <$> f l <*> f r

instance
  Show a =>
  Show (Pcm Stereo a)
  where
  showsPrec d (MkStereo l r) =
    showParen
      (d > 10)
      (showChar 'L' . showsPrec 11 l . showString " R" . showsPrec 11 r)

instance
  IsPcmValue a =>
  IsPcmValue (Pcm Stereo a)
  where
  pcmAverage (MkStereo !l0 !r0) (MkStereo !l1 !r1) =
    MkStereo (pcmAverage l0 l1) (pcmAverage r0 r1)

instance
  CanBeBlank a =>
  CanBeBlank (Pcm Stereo a)
  where
  blank = MkStereo blank blank

instance
  Default a =>
  Default (Pcm Stereo a)
  where
  def = MkStereo def def

instance
  forall s.
  Storable s =>
  Storable (Pcm Stereo s)
  where
  sizeOf _ = 2 * sizeOf (undefined :: s)
  alignment = alignment
  peekByteOff ptr off = do
    let
      sPtr :: Ptr s
      sPtr = castPtr ptr
    l <- peekByteOff sPtr off
    let rOffset = sizeOf l
    r <- peekByteOff sPtr (rOffset + off)
    return (MkStereo l r)
  pokeByteOff ptr off (MkStereo l r) = do
    let
      sPtr :: Ptr s
      sPtr = castPtr ptr
    pokeByteOff sPtr off l
    let rOffset = sizeOf l
    pokeByteOff sPtr (off + rOffset) r
