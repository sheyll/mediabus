module Data.MediaBus.DebugSink
    ( debugAudioPlaybackSink ) where

import           Data.MediaBus.Media
import           Data.MediaBus.Media.Audio.Raw
import           Data.MediaBus.Media.Audio
import           Data.MediaBus.Media.Channels
import           Data.MediaBus.Media.Buffer
import           Data.MediaBus.Ticks
import           Data.MediaBus.Stream
import           System.IO               ( Handle, hClose )
import           System.Process          ( shell )
import           Data.Streaming.Process  ( Inherited(..), streamingProcess
                                         , waitForStreamingProcess )
import qualified Data.ByteString         as B
import           Data.Conduit
import           Control.Monad.IO.Class
import           Data.Proxy
import           Text.Printf
import           Data.Default
import           Control.Lens


debugAudioPlaybackSink
  :: forall  m i s t p c r ch pcm . (Default i, MonadIO m, KnownRate r, KnownChannelLayout ch, IsPcmValue (Pcm ch pcm)
         , HasMediaL' c (Audio r ch (Raw pcm))
         , HasMediaBuffer' (Audio r ch (Raw pcm))) =>
         Sink (Stream i s t p c) m ()
debugAudioPlaybackSink = toFramesC .|
      do
        let cp = shell (printf "play -r %d -b 16 -c1  -e signed-integer -t raw -"
                               (rateVal (Proxy :: Proxy r)))
        (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
        awaitForever (mapMOf_ (framePayload . media' . mediaBuffer') (pcmToByteString sinH))
        liftIO (hClose sinH)
        _ <- waitForStreamingProcess cph
        return ()
      where
        pcmToByteString !h !d =
            liftIO (B.hPut h (mediaBufferToByteString d))
