module Data.MediaBus.DebugSink
  ( debugAudioPlaybackSink
  ) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Conduit
import Data.Default
import Data.MediaBus.Media
import Data.MediaBus.Media.Audio
import Data.MediaBus.Media.Audio.Raw
import Data.MediaBus.Media.Buffer
import Data.MediaBus.Media.Channels
import Data.MediaBus.Stream
import Data.MediaBus.Basics.Ticks
import Data.Proxy
import Data.Streaming.Process
       (Inherited(..), streamingProcess, waitForStreamingProcess)
import System.IO (Handle, hClose)
import System.Process (shell)
import Text.Printf

debugAudioPlaybackSink
  :: forall m i s t p c r ch pcm.
     ( Default i
     , MonadIO m
     , KnownRate r
     , KnownChannelLayout ch
     , IsPcmValue (Pcm ch pcm)
     , HasMediaL' c (Audio r ch (Raw pcm))
     , HasMediaBuffer' (Audio r ch (Raw pcm))
     )
  => Sink (Stream i s t p c) m ()
debugAudioPlaybackSink =
  toFramesC .| do
    let cp =
          shell
            (printf
               "play -r %d -b 16 -c1  -e signed-integer -t raw -"
               (rateVal (Proxy :: Proxy r)))
    (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
    awaitForever
      (mapMOf_
         (framePayload . media' . mediaBuffer')
         (pcmToByteString sinH))
    liftIO (hClose sinH)
    _ <- waitForStreamingProcess cph
    return ()
  where
    pcmToByteString !h !d = liftIO (B.hPut h (mediaBufferToByteString d))
