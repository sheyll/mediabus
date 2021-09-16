module Data.MediaBus.Media.Audio.RawSpec (spec) where

import Control.Lens
import Control.Monad (forM)
import Data.Data (Proxy (Proxy))
import Data.MediaBus
import Foreign (Storable (peekByteOff, pokeByteOff, sizeOf), allocaArray)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Storable" $ do
    describe "storing a buffer won't change the buffer" $ do
      it "Audio (Hz 48000) Mono (Raw S16)" $ property (p1 @Mono @S16)
      it "Audio (Hz 48000) Stereo (Raw S16)" $ property (p1 @Stereo @S16)
      it "Audio (Hz 48000) Mono (Raw Alaw)" $ property (p1 @Mono @ALaw)
      it "Audio (Hz 48000) Stereo (Raw Alaw)" $ property (p1 @Stereo @ALaw)

  describe "CanGenerateBlankMedia" $ do
    describe "for each duration generates a value such that getDuration equals that duration" $ do
      it "Audio (Hz 48000) Mono (Raw S16)" $ property (p2 (Proxy @Mono) (Proxy @S16))
      it "Audio (Hz 48000) Stereo (Raw S16)" $ property (p2 (Proxy @Stereo) (Proxy @S16))
      it "Audio (Hz 48000) Mono (Raw ALaw)" $ property (p2 (Proxy @Mono) (Proxy @ALaw))
      it "Audio (Hz 48000) Stereo (Raw ALaw)" $ property (p2 (Proxy @Stereo) (Proxy @ALaw))

p1 :: forall c t. (Show (Pcm c t), CanBeSample (Pcm c t)) => Audio (Hz 48000) c (Raw t) -> Property
p1 inAudio = do
  let len = mediaBufferLength inBuf
      inBuf = inAudio ^. mediaBufferLens
  ioProperty $
    allocaArray @(Pcm c t) len $ \bufPtr -> do
      outList <- forM [0 .. len - 1] $ \i -> do
        let offset = i * sizeOf (undefined :: Pcm c t)
        mapM_ (pokeByteOff bufPtr offset) (inBuf ^? ix i)
        peekByteOff bufPtr offset
      return $ mediaBufferToList inBuf === outList

p2 ::
  forall c t.
  (Show (Pcm c t), CanBeBlank (Pcm c t), CanBeSample (Pcm c t)) =>
  Proxy c ->
  Proxy t ->
  Positive Integer ->
  Property
p2 _ _ (Positive someDuration) =
  let x :: Audio (Hz 48000) c (Raw t)
      x = blankFor (fromInteger someDuration)

      epsilonTicks :: Ticks (Hz 48000) Integer
      epsilonTicks = MkTicks 1

      epsilon = epsilonTicks ^. nominalDiffTime
   in fromIntegral someDuration - epsilon <= getDuration x
        .&&. getDuration x < fromIntegral someDuration + epsilon
