module Data.MediaBus.Media.BufferSpec (spec) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.MediaBus
import qualified Data.Vector.Generic.Mutable as V
import Test.Hspec

spec :: Spec
spec = describe "MediaBuffer" $ do
  it "can be mapped over with eachSample" $
    (mediaBufferFromList "Hello" & each %~ toUpper)
      `shouldBe` mediaBufferFromList "HELLO"
  it "can be mapped over with eachSample changing the type" $
    ( mediaBufferFromList "Hello" & mediaBufferVector
        . each
        %~ const True
    )
      `shouldBe` mediaBufferFromList (Prelude.replicate 5 True)
  describe "modifyMediaBuffer"
    $ it "modifies in-place"
    $ let f v =
            -- imperative safe destructive updates
            let n = V.length v
             in forM_ [0 .. (n - 1) `div` 2] (\i -> V.swap v i (n - 1 - i))
       in modifyMediaBuffer f (mediaBufferFromList [1 .. 4 :: Int])
            `shouldBe` mediaBufferFromList [4, 3 .. 1]
  describe "unsafeModifyMediaBuffer"
    $ it "modifies in-place and can return values"
    $ let f v =
            -- imperative safe destructive updates
            let n = V.length v
             in forM
                  [0 .. (n - 1) `div` 2]
                  ( \i -> do
                      V.swap v i (n - 1 - i)
                      return i
                  )
       in unsafeModifyMediaBuffer f (mediaBufferFromList [1 .. 4 :: Int])
            `shouldBe` ([0, 1], mediaBufferFromList [4, 3 .. 1])

data TestFormat = MkTestFormat
  deriving (Show)
