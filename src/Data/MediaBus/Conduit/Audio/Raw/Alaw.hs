-- | Conduits for converting 'ALaw' 'Stream's to 'S16' streams and vice versa.
module Data.MediaBus.Conduit.Audio.Raw.Alaw
  ( alawToS16
  , s16ToAlaw
  )
where

import           Control.Lens
import           Control.DeepSeq                ( NFData )
import           Data.Conduit
import           Data.MediaBus.Media.Audio.Raw.Alaw
import           Data.MediaBus.Media.Audio.Raw.Signed16bit
import           Data.MediaBus.Media.Channels
import           Data.MediaBus.Media.Stream
import           Data.MediaBus.Conduit.Stream

-- | Convert from 'ALaw' to 'S16'
alawToS16
  :: (NFData cIn, NFData cOut, Monad m, EachChannelL cIn cOut ALaw S16)
  => ConduitT (Stream i s t p cIn) (Stream i s t p cOut) m ()
alawToS16 = mapFrameContentC' (over eachChannel decodeALawSample)

-- | Convert from 'S16' to 'ALaw'
s16ToAlaw
  :: (NFData cIn, NFData cOut, Monad m, EachChannelL cIn cOut S16 ALaw)
  => ConduitT (Stream i s t p cIn) (Stream i s t p cOut) m ()
s16ToAlaw = mapFrameContentC' (over eachChannel encodeALawSample)
