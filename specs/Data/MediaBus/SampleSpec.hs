module Data.MediaBus.SampleSpec ( spec ) where

import           Data.MediaBus
import           Control.Monad
import           Test.Hspec
import           Control.Lens
import           Data.Char
import qualified Data.Vector.Generic.Mutable as V

spec :: Spec
spec = describe "SampleBuffer" $ do
    it "can be mapped over with eachSample" $
        (sampleBufferFromList "Hello" & eachSample %~ toUpper) `shouldBe`
        sampleBufferFromList "HELLO"
    it "can be mapped over with eachSample changing the type" $
        (sampleBufferFromList "Hello" & sampleBuffer . sampleVector .
             each %~
             const True) `shouldBe`
        sampleBufferFromList (Prelude.replicate 5 True)
    describe "mutateSamples" $
        it "modifies in-place" $
        let f v =
                -- imperative safe destructive updates
                let n = V.length v
                in
                    forM_ [0 .. (n - 1) `div` 2] (\i -> V.swap v i (n - 1 - i))
        in
            mutateSamples f (sampleBufferFromList [1 .. 4 :: Int]) `shouldBe`
                sampleBufferFromList [4,3 .. 1]
    describe "unsafeMutateSamples" $
        it "modifies in-place and can return values" $
        let f v =
                -- imperative safe destructive updates
                let n = V.length v
                in
                    forM [0 .. (n - 1) `div` 2]
                         (\i -> do
                              V.swap v i (n - 1 - i)
                              return i)
        in
            unsafeMutateSamples f (sampleBufferFromList [1 .. 4 :: Int]) `shouldBe`
                ([ 0, 1 ], sampleBufferFromList [4,3 .. 1])

data TestFormat = MkTestFormat
    deriving Show
