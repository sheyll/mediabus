module Data.MediaBus.Conduit.FrameRingSpec (spec) where

import Conduit
import Control.Monad
import Data.Conduit.List as Conduit
import Data.MediaBus
import Data.Proxy
import Data.Word
import Debug.Trace
import Test.Hspec
import Test.QuickCheck
import Data.Default
import Control.Concurrent.Async
import FakePayload
import Control.Lens

spec :: Spec
spec =
  describe "FrameRing" $ do
    describe "basics" $ do
      it "can be created with mkFrameRing" $ void $ mkFrameRing @IO @() 1
      context "no input data" $ do
        it
          "it generates single start frame" $ do
            r <- mkFrameRing @IO @Int @() @FakePayload 10
            let
              initialStartFrame :: SyncStream Int () FakePayload
              initialStartFrame = MkStream (Start (MkFrameCtx def def def def))
              source = frameRingSource r 0.001
            out' <- connect source await
            let out = set (_Just . stream . _Start . frameCtxSourceId) 0 out'
            out `shouldBe` Just (Got <$> initialStartFrame)

        it
          "it generates enough Missing frames" $ do
            r <- mkFrameRing @IO @Int @() @FakePayload 10
            let
              expected :: SyncStream Int () (Discontinous FakePayload)
              expected = MkStream (Next (MkFrame def def Missing))
              source = frameRingSource r 0.001
            (_:outs) <- runConduit (source .| Conduit.takeExactlyC 5 sinkList)
            Control.Monad.mapM_ (`shouldBe` expected) outs

      it
        "passes a single frame from a frameRingSink to a frameRingSource" $ do
          r <- mkFrameRing @IO  10
          let
            testStartFrame :: SyncStream Int () FakePayload
            testStartFrame = MkStream (Start (MkFrameCtx def def def def))
            sink = frameRingSink r
            source = frameRingSource r 0.001
          -- (_, out) <- concurrently
          --   (connect (yield testStartFrame) sink)
          ---  (connect source await)
          connect (yield testStartFrame) sink
          out' <- connect source await
          let out = set (_Just . stream . _Start . frameCtxSourceId) def out'
          out `shouldBe` Just (Got <$> testStartFrame)

    describe "underflow" $ do
      it "will send missing packages with the right frequency" pending

