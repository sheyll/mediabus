module Data.MediaBus.TicksSpec ( spec ) where

import           Data.MediaBus
import           Test.QuickCheck
import           Test.Hspec
import           Control.Lens
import           Data.Word

spec :: Spec
spec = do
    describe "nominalDiffTime At8kHzU32" $
        it "is isomorphic" $
            property $
                \tix -> (view (from nominalDiffTime)
                              (view nominalDiffTime (mkTicks at8kHzU32 tix)))
                        `shouldBe`
                        mkTicks at8kHzU32 tix
    describe "nominalDiffTime At48kHzU64" $
        it "is isomorphic" $
            property $
                \tix -> (view (from nominalDiffTime)
                              (view nominalDiffTime (mkTicks at48kHzU64 tix)))
                        `shouldBe`
                        mkTicks at48kHzU64 tix
    describe "convertTicks between At16kHzU32 and At16kHzU32" $
        it "is idempotent" $
            property $
                \tix -> let t = mkTicks at16kHzU32 tix
                        in
                            convertTicks t `shouldBe` t
    describe "convertTicks between At8kHzU32 and At16kHzU64" $
        it "preserves nominalDiffTime when converting back and forth" $
            property $
                \tix -> let t = mkTicks at8kHzU32 tix
                        in
                            view nominalDiffTime
                                 (convertTicks t :: Ticks 16000 Word64) `shouldBe`
                                view nominalDiffTime t
