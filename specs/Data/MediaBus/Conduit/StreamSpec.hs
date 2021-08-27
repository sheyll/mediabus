module Data.MediaBus.Conduit.StreamSpec (spec) where

import Conduit
import Control.Lens
import Control.Monad.Logger
import Data.Conduit.List
import Data.MediaBus
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Stream functions" $
    do
      describe "isStartFrame" $
        it "returns True for start frames and False otherwise" $  do
          isStartFrame (MkStream (Start (MkFrameCtx () () () ()))) `shouldBe` True
          isStartFrame (MkStream (Next (MkFrame () () ()))) `shouldBe` False
      describe "isPayloadFrame" $
        it "returns True for payload frames and False otherwise" $  do
          isPayloadFrame (MkStream (Start (MkFrameCtx () () () ()))) `shouldBe` False
          isPayloadFrame (MkStream (Next (MkFrame () () ()))) `shouldBe` True

  describe "Stream conduits" $
    do
      describe "logStreamC" $ do
        it "passes through the values when no logging is performced" $
          property $ \(inputs :: [Stream Bool Int Int Int Int]) -> ioProperty $ do
            outputs <-
              runNoLoggingT
                . runResourceT
                . runConduit
                $ (yieldMany inputs .| logStreamC (const Nothing) "test" .| consume)
            return (inputs === outputs)
        it "passes through the values when logging is performced" $
          property $ \(inputs :: [Stream Bool Int Int Int Int]) -> ioProperty $ do
            outputs <-
              runNoLoggingT
                . runResourceT
                . runConduit
                $ (yieldMany inputs .| logStreamC (const (Just LevelWarn)) "test" .| consume)
            return (inputs === outputs)
      describe "assumeSynchronized" $
        it "removes sequence numbers and timestamps but passes all buffers untouched" $
          property $ \(inputs :: [Stream Bool Int Int Int Int]) ->
            let outputs = runConduitPure (yieldMany inputs .| assumeSynchronizedC .| sinkList)
             in length inputs === length outputs
                  .&&. toListOf (each . eachFramePayload) inputs
                    === toListOf (each . eachFramePayload) outputs
