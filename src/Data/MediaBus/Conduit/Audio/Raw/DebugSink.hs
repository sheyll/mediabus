-- | A small utility module that sends an audio stream via /stdout/ to a @sox@
-- system command that plays the audio.
module Data.MediaBus.Conduit.Audio.Raw.DebugSink
  ( debugAudioPlaybackSink
  )
where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Conduit
import Data.Default
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Audio
import Data.MediaBus.Media.Audio.Raw
import Data.MediaBus.Media.Buffer
import Data.MediaBus.Media.Channels
import Data.MediaBus.Media.Media
import Data.MediaBus.Media.Stream
import Data.Proxy
import Data.Streaming.Process
  ( Inherited (..),
    streamingProcess,
    waitForStreamingProcess,
  )
import System.IO
  ( Handle,
    hClose,
  )
import System.Process (shell)
import Text.Printf
import Control.Monad.Logger (logInfo, MonadLogger)

-- | A 'Sink' that launches a shell command that starts @sox@ such that it reads
-- raw audio data from @STDIN@ and plays it via the systems sound card.
debugAudioPlaybackSink ::
  forall m i s t p c r ch pcm.
  ( Default i,
    MonadIO m,
    KnownRate r,
    KnownChannelLayout ch,
    IsPcmValue (Pcm ch pcm),
    HasMediaL' c (Audio r ch (Raw pcm)),
    HasMediaBuffer' (Audio r ch (Raw pcm)), MonadLogger m) =>
  ConduitT (Stream i s t p c) Void m ()
debugAudioPlaybackSink = toFramesC .| do
  let cp =
        shell
          ( printf
              "play -r %d -b 16 -c1  -e signed-integer -t raw -"
              (rateVal (Proxy :: Proxy r))
          )
  $logInfo "launched external process to playback audio"
  (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
  awaitForever
    (mapMOf_ (framePayload . media' . mediaBuffer') (pcmToByteString sinH))
  liftIO (hClose sinH)
  $logInfo "closing pipe to audio playback process"
  _ <- waitForStreamingProcess cph
  return ()
  where
    pcmToByteString !h !d =
      liftIO (B.hPut h (mediaBufferToByteString d))
