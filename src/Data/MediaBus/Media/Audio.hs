-- | This module centers around the data family 'Audio' that provides general
-- info about an audio stream, such as sampling rate and channel layout and
-- codec.
module Data.MediaBus.Media.Audio
  ( Audio,
  )
where

import Control.Lens (iso)
import Data.MediaBus.Basics.Ticks (HasRate (..), KnownRate, Rate)
import Data.MediaBus.Media.Channels
  ( HasChannelLayout (..),
    KnownChannelLayout,
  )
import Data.MediaBus.Media.Media (HasMedia (..), IsMedia)

-- | Family of audio media types, indexed by a sampleRate, a channel layout
--   and a /codec/ parameter. This type family is intended as a basis for all audio
--   media, raw as well as encoded audio.
data family Audio (sampleRate :: Rate) channelLayout encoding

instance
  (KnownRate r) =>
  HasRate (Audio r c t)
  where
  type GetRate (Audio r c t) = r
  type SetRate (Audio r c t) x = Audio x c t

instance
  (KnownChannelLayout c) =>
  HasChannelLayout (Audio r c t)
  where
  type ChannelLayout (Audio r c t) = c
  type SetChannelLayout (Audio r c t) c' = Audio r c' t

instance
  (IsMedia (Audio r c e), IsMedia (Audio r' c' e')) =>
  HasMedia (Audio r c e) (Audio r' c' e')
  where
  type MediaFrom (Audio r c e) = Audio r c e
  type MediaTo (Audio r' c' e') = Audio r' c' e'
  media = iso id id
