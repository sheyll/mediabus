module Data.MediaBus.Media.Audio
  ( Audio
  ) where

import Data.MediaBus.Media
import Data.MediaBus.Media.Channels
import Data.MediaBus.Ticks
import Control.Lens

-- | Family of audio media types, indexed by the sampleRate, the channel layout
--   and a /codec/ parameter. This class is intended as a basis for all audio
--   media, raw as well as encoded audio.
data family Audio (sampleRate :: Rate) channelLayout encoding

instance (KnownRate r) =>
         HasRate (Audio r c t) where
  type GetRate (Audio r c t) = r
  type SetRate (Audio r c t) x = Audio x c t

instance (KnownChannelLayout c) =>
         HasChannelLayout (Audio r c t) where
  type ChannelLayout (Audio r c t) = c
  type SetChannelLayout (Audio r c t) c' = Audio r c' t

instance (IsMedia (Audio r c e), IsMedia (Audio r' c' e')) =>
  HasMedia (Audio r c e) (Audio r' c' e') where
    type MediaFrom (Audio r c e) = Audio r c e
    type MediaTo (Audio r' c' e') = Audio r' c' e'
    media = iso id id
