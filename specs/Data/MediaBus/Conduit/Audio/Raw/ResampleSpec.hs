module Data.MediaBus.Conduit.Audio.Raw.ResampleSpec
  ( spec
  ) where

import Conduit
import Control.Monad.State.Strict
import Data.Default
import Data.MediaBus
import Test.Hspec
import Test.QuickCheck
import Control.Lens

spec :: Spec
spec =
  describe "Resampling of S16 samples from 8 to 16 kHz" $ do
    it "interpolates between samples" $
      let lastVal = 0
      in property $ \(NonEmpty samples) ->
           mediaBufferToList
             (view
                pcmMediaBuffer
                (resampleAndConsume (singleFrameFromList samples) lastVal)) `shouldBe`
           expectedResamplingResult samples lastVal
    it "interpolates also between frames" $
      let lastVal = 0
      in property $ \samplesLists ->
           mediaBufferToList
             (view
                pcmMediaBuffer
                (resampleAndConsume (framesFromLists samplesLists) lastVal)) `shouldBe`
           expectedResamplingResult (join samplesLists) lastVal

expectedResamplingResult :: [Pcm Mono S16] -> Pcm Mono S16 -> [Pcm Mono S16]
expectedResamplingResult xs lastVal =
  concatMap (\(x, y) -> [pcmAverage x y, y]) (zip (lastVal : xs) xs)

type AudioBuffer r = Audio (Hz r) Mono (Raw S16)

resampleAndConsume
  :: Source Identity (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000))
  -> Pcm Mono S16
  -> AudioBuffer 16000
resampleAndConsume vvv lastVal =
  runConduitPure (vvv .| resample8to16kHz' lastVal .| concatStreamContents)

singleFrameFromList
  :: Monad m
  => [Pcm Mono S16]
  -> Source m (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000))
singleFrameFromList x =
  mapOutput
    (MkStream . Next)
    (mapOutput (MkFrame () def) (yield (pcmMediaBuffer # mediaBufferFromList x)) .|
     setTimestampFromDurationsC 0)

framesFromLists
  :: Monad m
  => [[Pcm Mono S16]]
  -> Source m (Stream SrcId32 SeqNum32 Ticks32At48000 () (AudioBuffer 8000))
framesFromLists xs =
  mapOutput
    (MkStream . Next)
    (mapOutput (MkFrame () def) (mapM_ (yield . view (from pcmMediaBuffer) . mediaBufferFromList) xs) .|
     setTimestampFromDurationsC 0)
