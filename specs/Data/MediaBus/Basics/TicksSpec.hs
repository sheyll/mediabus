module Data.MediaBus.Basics.TicksSpec
  ( spec,
  )
where

import Control.Lens
import Data.MediaBus
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "nominalDiffTime At8kHzU32"
    $ it "is isomorphic"
    $ property
    $ \tix ->
      view (from nominalDiffTime) (view nominalDiffTime (mkTicks32At8000 tix))
        `shouldBe` mkTicks32At8000 tix
  describe "nominalDiffTime At48kHzU64"
    $ it "is isomorphic"
    $ property
    $ \tix ->
      view (from nominalDiffTime) (view nominalDiffTime (mkTicks64At48000 tix))
        `shouldBe` mkTicks64At48000 tix
  describe "convertTicks between At16kHzU32 and At16kHzU32"
    $ it "is idempotent"
    $ property
    $ \tix ->
      let t = mkTicks32At16000 tix
       in convertTicks t `shouldBe` t
  describe "convertTicks between At8kHzU32 and At16kHzU64"
    $ it "preserves nominalDiffTime when converting back and forth"
    $ property
    $ \tix ->
      let t = mkTicks32At8000 tix
       in view nominalDiffTime (convertTicks t :: Ticks (Hz 16000) Word64)
            `shouldBe` view nominalDiffTime t
