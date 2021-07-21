module Data.MediaBus.Conduit.FrameRingSpec (spec) where

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
  describe "FrameRing" $ do
    describe "basics" $ do
      it "can be created with mkFrameRing" $ void $ mkFrameRing @IO @() 1
      it
        "passes a single frame from a frameRingSink to a frameRingSource"
        pending
    describe "underflow" $ do
      it "will send missing packages with the right frequency" pending
