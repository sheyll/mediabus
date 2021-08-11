module Data.MediaBus.Media.Audio.RawSpec (spec) where

import Control.Lens
import Data.MediaBus
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "CanGenerateBlankMedia" $ do
    describe "Audio (Hz 48000) Mono (Raw S16)" $
      it "for each duration generates a value such that getDuration equals that duration" $
        property
          ( \(Positive someDuration) ->
              let x :: Audio (Hz 48000) Mono (Raw S16)
                  x = blankFor (fromInteger someDuration)

                  epsilonTicks :: Ticks (Hz 48000) Integer
                  epsilonTicks = MkTicks 1

                  epsilon = epsilonTicks ^. nominalDiffTime
               in fromIntegral someDuration - epsilon <= getDuration x
                    .&&. getDuration x < fromIntegral someDuration + epsilon
          )
    describe "Audio (Hz 48000) Stereo (Raw S16)" $
      it "for each duration generates a value such that getDuration equals that duration" $
        property
          ( \(Positive someDuration) ->
              let x :: Audio (Hz 48000) Stereo (Raw S16)
                  x = blankFor (fromInteger someDuration)

                  epsilonTicks :: Ticks (Hz 48000) Integer
                  epsilonTicks = MkTicks 1

                  epsilon = epsilonTicks ^. nominalDiffTime
               in fromIntegral someDuration - epsilon <= getDuration x
                    .&&. getDuration x < fromIntegral someDuration + epsilon
          )
