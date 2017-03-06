-- |  Channel representation types.
module Data.MediaBus.Media.Channels
    ( KnownChannelLayout(..)
    , ChannelLayoutProxy(..)
    , HasChannelLayout(..)
    , EachChannel(..)
    , type EachChannelL
    , type EachChannelL'
    , type EachChannel'
    , eachChannel'
    ) where

import           Control.Lens
import           Data.Typeable

-- | Like 'KnownNat' but for promoted 'ChannelLayout's.
class Typeable c =>
      KnownChannelLayout c where
    -- | Return the 'ChannelLayout' for type @c@
    numberOfChannels :: proxy c -> Int

-- | A Proxy specifically to prevent orphan instances of e.g. @Proxy channelLayoyt@.
data ChannelLayoutProxy c where
        MkChannelLayoutProxy :: ChannelLayoutProxy c
        ConvertChannelLayoutProxy :: proxy c -> ChannelLayoutProxy c

-- | Create a textual representation of the channel layout.
instance KnownChannelLayout c =>
         Show (ChannelLayoutProxy c) where
    showsPrec d px = showParen (d > 10)
                               (showsPrec 11 (typeRep px) .
                                    showString " " .
                                        showsPrec 11 (numberOfChannels px))

-- | Types that have some 'KnownChannelLayout'
class (SetChannelLayout s (ChannelLayout s) ~ s) =>
      HasChannelLayout s where
    -- | The channel layout contained in 's'
    type ChannelLayout s
    -- | The type resulting from changing the channel layout type in 's' to 'b'
    type SetChannelLayout s b

-- | Types, e.g samples, that have one or more channels
class EachChannel s t where
    -- | The channel layout contained in 's'
    type ChannelsFrom s
    -- | The channel layout contained in 't'
    type ChannelsTo t

    --  | Traversal for accessing each individual channel
    eachChannel :: Traversal s t (ChannelsFrom s) (ChannelsTo t)

-- | A constraint type alias for 'EachChannel' similar to the parameters of type class 'Each' from the lens package.
type EachChannelL s t a b = (EachChannel s t, ChannelsFrom s ~ a, ChannelsTo t ~ b)

-- | A constraint type alias for 'EachChannel' with a simple traversal
type EachChannelL' s a = (EachChannel s s, ChannelsFrom s ~ a, ChannelsTo s ~ a)

-- | A simple 'Traversal' for the channels in samples
type EachChannel' s = (ChannelsFrom s ~ ChannelsTo s, EachChannel s s)

-- | A simple 'Traversal' for the channels in samples
eachChannel' :: EachChannel' i => Traversal' i (ChannelsFrom i)
eachChannel' = eachChannel
