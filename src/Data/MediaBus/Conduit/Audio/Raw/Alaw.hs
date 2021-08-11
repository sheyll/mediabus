-- | Conduits for converting 'ALaw' 'Stream's to 'S16' streams and vice versa.
module Data.MediaBus.Conduit.Audio.Raw.Alaw
  ( alawToS16,
    s16ToAlaw,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens (over)
import Data.Conduit (ConduitT)
import Data.MediaBus.Conduit.Stream (mapFrameContentC')
import Data.MediaBus.Media.Audio.Raw.Alaw
  ( ALaw,
    decodeALawSample,
    encodeALawSample,
  )
import Data.MediaBus.Media.Audio.Raw.Signed16bit (S16)
import Data.MediaBus.Media.Channels
  ( EachChannel (eachChannel),
    EachChannelL,
  )
import Data.MediaBus.Media.Stream (Stream)

-- | Convert from 'ALaw' to 'S16'
alawToS16 ::
  (NFData cIn, NFData cOut, Monad m, EachChannelL cIn cOut ALaw S16) =>
  ConduitT (Stream i s t p cIn) (Stream i s t p cOut) m ()
alawToS16 = mapFrameContentC' (over eachChannel decodeALawSample)

-- | Convert from 'S16' to 'ALaw'
s16ToAlaw ::
  (NFData cIn, NFData cOut, Monad m, EachChannelL cIn cOut S16 ALaw) =>
  ConduitT (Stream i s t p cIn) (Stream i s t p cOut) m ()
s16ToAlaw = mapFrameContentC' (over eachChannel encodeALawSample)
