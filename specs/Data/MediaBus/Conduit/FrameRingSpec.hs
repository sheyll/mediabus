{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
module Data.MediaBus.Conduit.FrameRingSpec (spec) where

import Conduit
import Control.Monad as Monad
import Data.MediaBus
import Test.Hspec
import Data.Default
import Control.Concurrent.Async
import FakePayload
import Control.Lens
import Data.Time.Clock (NominalDiffTime)
import Control.Concurrent (threadDelay)
import Test.QuickCheck

spec :: Spec
spec =
  describe "FrameRing" $ do
    describe "basics" $ do
      it "can be created with mkFrameRing" $ void $ mkFrameRing @IO @Int @()  1
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
            Monad.mapM_ (`shouldBe` expected) outs

      context "regular input data" $ do
        it
          "passes a single start frame from a frameRingSink to a frameRingSource" $ do
            r <- mkFrameRing @IO  10
            let
              testStartFrame :: SyncStream Int () FakePayload
              testStartFrame = MkStream (Start (MkFrameCtx 777 def def def))
              sink = frameRingSink r
              source = frameRingSource r 0.02
            (_, Just out0) <- concurrently
              (connect (yield testStartFrame) sink)
              (runConduit (source .| do { void await; await }))
            out0 `shouldBe` Missing <$ testStartFrame

        it
          "passes 3 payload frames from a frameRingSink to a frameRingSource" $ do
            r <- mkFrameRing @IO  10
            let
              f1 :: SyncStream Int () FakePayload
              f1 = MkStream (Next (MkFrame def def (FP (1 + review nominalDiffTime duration))))
              f2 = MkStream (Next (MkFrame def def (FP (2 + review nominalDiffTime duration))))
              f3 = MkStream (Next (MkFrame def def (FP (3 + review nominalDiffTime duration))))
              sink = frameRingSink r
              duration :: NominalDiffTime
              duration = 0.001
              source = frameRingSource r duration
            (_, [_, out1,out2,out3]) <- concurrently
              (connect (yieldMany [f1,f2,f3]) sink)
              (runConduit (source .| Conduit.takeExactlyC 4 sinkList))
            out1 `shouldBe` Got <$> f1
            out2 `shouldBe` Got <$> f2
            out3 `shouldBe` Got <$> f3

        it "when the ring size is > 1 and no overflow happens, all input packets are forwarded through the ring" $
          property $ \inputsBool (Positive qLen) ->
           qLen >= 2 && qLen > length inputsBool ==> ioProperty $ do
            let
              duration :: NominalDiffTime
              duration = 0.001
            let
              f,s :: Int -> SyncStream Int () FakePayload
              f i = MkStream (Next (MkFrame def def (FP (fromIntegral i + review nominalDiffTime duration))))
              s i = MkStream (Start (MkFrameCtx i def def def))
              inputsSF = take (length inputsBool) (map (\isStart -> if isStart then s else f) inputsBool)
              input = zipWith ($) inputsSF [1..]
            r <- mkFrameRing @IO (fromIntegral qLen)
            let
              f',s' :: Int -> SyncStream Int () (Discontinous FakePayload)
              f' i = MkStream (Next (MkFrame def def (Got (FP (fromIntegral i + review nominalDiffTime duration)))))
              s' i = MkStream (Start (MkFrameCtx i def def def))
              expectedSF = take (length inputsBool) (map (\isStart -> if isStart then s' else f') inputsBool)
              expected = zipWith ($) expectedSF [1..]
            let
              sink = frameRingSink r
              source = frameRingSource r duration
            connect (yieldMany input) sink
            outputs <- runConduit (source .| Conduit.takeExactlyC (length expected + 1) sinkList)
            drop 1 outputs `shouldBe` expected

    describe "underflow" $ do
      it "constantly generates Missing frames if no input is send" $ property $
        \(Positive qLen) (Positive outputLen0) ->  qLen > 1 ==> ioProperty $ do
          let outputLen = qLen + outputLen0
          r <- mkFrameRing @IO (fromIntegral qLen)
          let source = frameRingSource r 0.001
              missing :: SyncStream Int () (Discontinous FakePayload)
              missing = MkStream (Next (MkFrame def def Missing))
          outputs <-  runConduit (source .| Conduit.takeExactlyC (1+outputLen) sinkList)
          tail outputs `shouldBe` replicate outputLen missing

      it
        "passes 1 payload frame, then generates a Missing because the sender stalls for more than a packet duration, then passes another frame" $ do
          r <- mkFrameRing @IO 4
          let
            f1 :: SyncStream Int () FakePayload
            f1 = MkStream (Next (MkFrame def def (FP (1 + review nominalDiffTime duration))))
            f3 = MkStream (Next (MkFrame def def (FP (3 + review nominalDiffTime duration))))
            sink = frameRingSink r
            duration :: NominalDiffTime
            duration = 0.02
            durationStalled = 30_000
            source = frameRingSource r duration
          (_, [_, out1,out2,out3,out4]) <- concurrently
            (connect
                (do
                  yield f1
                  liftIO (threadDelay durationStalled)
                  yield f3
                  )
                sink)
            (runConduit (source .| Conduit.takeExactlyC 5 sinkList))
          out1 `shouldBe` Got <$> f1
          out2 `shouldBe` Missing <$ f1
          out3 `shouldBe` Got <$> f3
          out4 `shouldBe` Missing <$ f1

    describe "overflow" $ do
      it "it keeps the initial start value and the most recent payloads when too many payloads are received" $
       property $ \(Positive (Small n)) ->
        (n > 1 && n <= 10) ==> ioProperty $ do
          r <- mkFrameRing @IO (fromIntegral n)
          let
            f1 :: SyncStream Int () FakePayload
            f1 = MkStream (Next (MkFrame def def (FP (1 + review nominalDiffTime duration))))
            sink = frameRingSink r
            duration :: NominalDiffTime
            duration = 0.002
            source = frameRingSource r duration
          connect (yieldMany (replicate (2*n) f1)) sink
          outs <- runConduit (source .| Conduit.takeExactlyC 2 sinkList)
          let
            outFirst :: SyncStream Int () (Discontinous FakePayload)
            outFirst = set (stream . _Start . frameCtxSourceId) 0 (head outs)
          return
            (outFirst === MkStream (Start (MkFrameCtx def def def def)))

      it "it keeps only the latest start value" $
       property $ \(Positive (Small n)) ->
        (n > 1 && n <= 10) ==> ioProperty $ do
          r <- mkFrameRing @IO 1 -- (fromIntegral n)
          let
            startFrames :: [SyncStream Int () FakePayload]
            startFrames = map (\i -> MkStream (Start (MkFrameCtx i () () ()))) [1 .. 2*n]
            sink = frameRingSink r
            duration :: NominalDiffTime
            duration = 0.002
            source = frameRingSource r duration
          connect (yieldMany startFrames) sink
          [outFirst] <- runConduit (source .| Conduit.takeExactlyC 1 sinkList)
          return
            (outFirst === (Missing <$ last startFrames))

    describe "overflow with queue lenght == 1" $ do
      it
        "it keeps the most recent start value and drops other frames" $ do
          r <- mkFrameRing @IO  1
          let
            f1,f2 :: SyncStream Int () FakePayload
            f1 = MkStream (Start (MkFrameCtx 777 def def def))
            f2 = MkStream (Next (MkFrame def def (FP (1 + review nominalDiffTime duration))))
            sink = frameRingSink r
            duration :: NominalDiffTime
            duration = 0.002
            source = frameRingSource r duration
          connect (yieldMany [f1,f1]) sink
          Just out <- runConduit (source .| await)
          out `shouldBe` Missing <$ f1

