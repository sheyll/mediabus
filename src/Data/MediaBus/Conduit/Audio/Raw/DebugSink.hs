-- | A small utility module that sends an audio stream via /stdout/ to a @sox@
-- system command that plays the audio.
module Data.MediaBus.Conduit.Audio.Raw.DebugSink
  ( debugAudioPlaybackSink,
    dumpToFile,
    dumpToMultiFile,
  )
where

import Control.Lens (mapMOf_, view)
import Control.Monad.Logger (MonadLogger)
import Data.MediaBus.InternalLogging
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, Void, awaitForever, yield, (.|))
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks (HasDuration (getDuration), KnownRate (rateVal))
import Data.MediaBus.Conduit.Stream (toFramesC)
import Data.MediaBus.Media.Audio (Audio)
import Data.MediaBus.Media.Audio.Raw (IsPcmValue, Pcm, Raw)
import Data.MediaBus.Media.Buffer
  ( HasMediaBufferLens',
    mediaBufferLens',
    mediaBufferToByteString,
  )
import Data.MediaBus.Media.Channels (KnownChannelLayout)
import Data.MediaBus.Media.Stream
import Data.Proxy (Proxy (..))
import Data.Streaming.Process
  ( Inherited (..),
    streamingProcess,
    waitForStreamingProcess,
  )
import System.Process (shell)
import Text.Printf
import UnliftIO
import UnliftIO.Directory (createDirectoryIfMissing)

-- | A 'Sink' that launches a shell command that starts @sox@ such that it reads
-- raw audio data from @STDIN@ and plays it via the systems sound card.
debugAudioPlaybackSink ::
  forall m i s t p c r ch pcm.
  ( MonadIO m,
    KnownRate r,
    KnownChannelLayout ch,
    IsPcmValue (Pcm ch pcm),
    HasMediaBufferLens' c,
    c ~ Audio r ch (Raw pcm),
    MonadLogger m
  ) =>
  ConduitT (Stream i s t p c) Void m ()
debugAudioPlaybackSink =
  toFramesC .| do
    let cp =
          shell
            ( printf
                "play -r %d -b 16 -c1  -e signed-integer -t raw -"
                (rateVal (Proxy :: Proxy r))
            )
    out "launched external process to playback audio"
    (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
    awaitForever
      (mapMOf_ (eachFramePayload . mediaBufferLens') (pcmToByteString sinH))
    hClose sinH
    out "closing pipe to audio playback process"
    _ <- waitForStreamingProcess cph
    return ()
  where
    pcmToByteString !h !d =
      liftIO (B.hPut h (mediaBufferToByteString d))

-- | Append the 'Frame' payloads to the given file.
dumpToFile ::
  forall m i s t p c.
  ( MonadUnliftIO m,
    MonadIO m,
    MonadLogger m,
    Show i,
    Show s,
    Show t,
    Show p,
    HasDuration c,
    HasMediaBufferLens' c
  ) =>
  FilePath ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
dumpToFile fname =
  awaitForever
    ( \x ->
        mapMOf_ (eachFramePayload . mediaBufferLens') appendMediaBuffer x
          >> yield x
    )
  where
    appendMediaBuffer !d =
      liftIO (B.appendFile fname (mediaBufferToByteString d))

-- --------------------------------

-- | Dump all 'Frame' and 'FrameCtx' values into seperate files in the given
-- directory.
dumpToMultiFile ::
  forall m i s t p c.
  ( MonadUnliftIO m,
    MonadIO m,
    MonadLogger m,
    Show i,
    Show s,
    Show t,
    Show p,
    HasDuration c,
    HasMediaBufferLens' c,
    Integral s,
    Integral t
  ) =>
  FilePath ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
dumpToMultiFile dirName = do
  let initialSubDirName = dirName <> "/start-undef"
  currentSubDirNameRef <- newIORef initialSubDirName
  createDirectoryIfMissing True initialSubDirName
  awaitForever
    ( \x ->
        mapMOf_ stream (liftIO . writeFrameToFile currentSubDirNameRef) x
          >> yield x
    )
  where
    writeFrameToFile currentSubDirNameRef = \case
      Start !f -> do
        let !subDirName =
              dirName <> "/start-"
                <> show (_frameCtxSourceId f)
            !fname =
              subDirName <> "-"
                <> printf "%010d" (toInteger (_frameCtxSeqNumRef f))
                <> "-"
                <> printf "%010d" (toInteger (_frameCtxTimestampRef f))
                <> ".txt"
        createDirectoryIfMissing True subDirName
        writeIORef currentSubDirNameRef subDirName
        writeFile fname (show (_frameCtxInit f))
      Next !f -> do
        !subDirName <- readIORef currentSubDirNameRef
        let !fname =
              subDirName <> "/"
                <> printf "%010d" (toInteger (_frameSeqNum f))
                <> "-"
                <> printf "%010d" (toInteger (_frameTimestamp f))
                <> "--"
                <> show (getDuration f)
            !mb = view mediaBufferLens' f
        liftIO (B.writeFile fname (mediaBufferToByteString mb))
