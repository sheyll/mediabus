module Data.MediaBus.Audio.Channels
    ( ChannelLayout(..)
    , numberOfChannels
    , HasChannelLayout(..)
    ) where

import           Control.Lens
import           Foreign.Storable
import           Data.MediaBus.Ticks
import           Data.MediaBus.BlankMedia
import           Test.QuickCheck
import           Data.Proxy
import           GHC.Generics             ( Generic )
import           Control.DeepSeq

-- | Data type for audio channel layouts
data ChannelLayout = SingleChannel | ChannelPair
    deriving (Show, Eq, Ord, Enum, Generic)

instance NFData ChannelLayout

-- | Return the number of samples required for the 'ChannelLayout'.
numberOfChannels :: ChannelLayout -> Int
numberOfChannels SingleChannel =
    1
numberOfChannels ChannelPair =
    2

-- | Like 'KnownNat' but for promoted 'ChannelLayout's.
class HasChannelLayout (c :: ChannelLayout) where
    -- | Return the 'ChannelLayout' for type @c@
    channelLayout :: proxy c -> ChannelLayout

instance HasChannelLayout 'SingleChannel where
    channelLayout _ = SingleChannel

instance HasChannelLayout 'ChannelPair where
    channelLayout _ = ChannelPair
