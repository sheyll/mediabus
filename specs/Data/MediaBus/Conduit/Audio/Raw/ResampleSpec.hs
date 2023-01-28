module Data.MediaBus.Conduit.Audio.Raw.ResampleSpec
  ( spec,
  )
where

import Conduit
import Control.Lens
import Control.Monad.State.Strict
import Data.Default
import Data.MediaBus
import Test.Hspec
import Test.QuickCheck
import Data.IORef

spec :: Spec
spec =
  describe "Resampling of S16 samples from 8 to 16 kHz" $ do
    it "interpolates between samples" $
      let lastVal = 0
       in property $
            \(NonEmpty samples) -> do
              out <- resampleAndConsume (singleFrameFromList samples) lastVal
              mediaBufferToList
                ( view
                    mediaBufferLens
                    out
                  )  `shouldBe` expectedResamplingResult samples lastVal
    it "interpolates also between frames" $
      let lastVal = 0
       in property $ \samplesLists -> do
            out <- resampleAndConsume (framesFromLists samplesLists) lastVal
            mediaBufferToList
              ( view
                  mediaBufferLens
                  out
              )
              `shouldBe` expectedResamplingResult (join samplesLists) lastVal

expectedResamplingResult :: [Pcm Mono S16] -> Pcm Mono S16 -> [Pcm Mono S16]
expectedResamplingResult xs lastVal =
  concatMap (\(x, y) -> [pcmAverage x y, y]) (zip (lastVal : xs) xs)

type AudioBuffer r = Audio (Hz r) Mono (Raw S16)

resampleAndConsume ::
  ConduitT () (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000)) IO () ->
  Pcm Mono S16 ->
  IO (AudioBuffer 16000)
resampleAndConsume vvv lastVal = do
  lvr <- newIORef lastVal
  runConduit (vvv .| resample8to16kHz' lvr .| concatStreamContents)

singleFrameFromList ::
  Monad m =>
  [Pcm Mono S16] ->
  ConduitT () (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000)) m ()
singleFrameFromList x =
  mapOutput
    (MkStream . Next)
    ( mapOutput (MkFrame () def) (yield (rawPcmAudioBuffer # mediaBufferFromList x))
        .| setTimestampFromDurationsC 0
    )

framesFromLists ::
  Monad m =>
  [[Pcm Mono S16]] ->
  ConduitT () (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000)) m ()
framesFromLists xs =
  mapOutput
    (MkStream . Next)
    ( mapOutput (MkFrame () def) (mapM_ (yield . view (from rawPcmAudioBuffer) . mediaBufferFromList) xs)
        .| setTimestampFromDurationsC 0
    )
