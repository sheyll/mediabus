module Data.MediaBus.Audio.Channels
    ( ChannelLayout(..)
    , ChannelPair(..)
    , leftSample
    , rightSample
    , HasChannelLayout(..)
    ) where

import           Control.Lens
import           Foreign.Storable
import           Data.MediaBus.Ticks
import           Data.MediaBus.BlankMedia
import           Test.QuickCheck
import           Data.Proxy
import           GHC.Generics        ( Generic )
import           Control.DeepSeq

data ChannelLayout = SingleChannel | ChannelPair
    deriving (Show, Eq, Ord, Enum, Generic)

instance (CanBeBlank r) =>
         CanBeBlank (ChannelPair r) where
    blank = MkChannelPair blank blank

instance NFData ChannelLayout

class HasChannelLayout c where
    channelLayout :: c -> ChannelLayout

data ChannelPair a = MkChannelPair { _leftSample  :: a
                                   , _rightSample :: a
                                   }
    deriving (Show, Eq, Ord, Generic)

instance NFData a =>
         NFData (ChannelPair a)

instance Arbitrary a =>
         Arbitrary (ChannelPair a) where
    arbitrary = MkChannelPair <$> arbitrary <*> arbitrary

makeLenses ''ChannelPair

instance HasChannelLayout a =>
         HasChannelLayout (ChannelPair a) where
    channelLayout MkChannelPair{_leftSample,_rightSample} =
        case (channelLayout _leftSample, channelLayout _rightSample) of
            (SingleChannel, SingleChannel) ->
                ChannelPair
            other -> error ("Sorry this channel layout is not supported: " ++
                                show other)

instance (HasDuration (Proxy a)) =>
         HasDuration (Proxy (ChannelPair a)) where
    getDuration _ = getDuration (Proxy :: Proxy a)

instance Storable s =>
         Storable (ChannelPair s) where
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
