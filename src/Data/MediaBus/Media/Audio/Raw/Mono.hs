-- | Single channel (Mono) PCM audio
module Data.MediaBus.Media.Audio.Raw.Mono
  ( Mono()
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Default
import Data.MediaBus.Media.Audio.Raw
import Data.MediaBus.Media.Blank
import Data.MediaBus.Media.Channels
import Data.Typeable
import Foreign.Storable
import Test.QuickCheck

-- | The channel layout indicator type for **mono** audio
data Mono

instance KnownChannelLayout Mono where
  numberOfChannels _ = 1

newtype instance  Pcm Mono s = MkMonoSample{_monoSample :: s}
                             deriving (CanBeBlank, NFData, Eq, Storable, Default, Typeable,
                                       Arbitrary, IsPcmValue, Num, Ord, Integral, Real, Enum)

instance (IsPcmValue a, IsPcmValue b) =>
         EachChannel (Pcm Mono a) (Pcm Mono b) where
  type ChannelsFrom (Pcm Mono a) = a
  type ChannelsTo (Pcm Mono b) = b
  eachChannel = iso _monoSample MkMonoSample

instance Show s =>
         Show (Pcm Mono s) where
  showsPrec d (MkMonoSample !s) =
    showParen (d > 10) $ showString "M " . showsPrec 11 s
