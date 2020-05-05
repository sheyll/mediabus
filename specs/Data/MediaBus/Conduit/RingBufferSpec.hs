module Data.MediaBus.Conduit.RingBufferSpec (spec) where

import Conduit
import Control.Monad
import Data.Conduit.List
import Data.MediaBus
import Data.Proxy
import Data.Word
import Debug.Trace
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "RingBuffer" $ do

    describe "basics"
      $ do
           it "can be created with newRingBuffer" $ do
             pending

           it "passes a single frame from a ringBufferSink to a ringBufferSource"
             pending


startFrame :: a -> Stream () a () () ()
startFrame sn = MkStream (Start (MkFrameCtx () () sn ()))

nextFrame :: a -> Stream () a () () ()
nextFrame sn = MkStream (Next (MkFrame () sn ()))
