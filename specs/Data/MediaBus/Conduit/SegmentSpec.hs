module Data.MediaBus.Conduit.SegmentSpec (spec) where

import Control.Lens
import Data.MediaBus
import Data.MediaBus.Conduit.SpecUtils
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "segmentC" $ do
  it "only outputs segments with a valid length"
    $ property
    $ \ls ->
      withTestData
        ls
        PT5
        ( \_ outputs ->
            Prelude.drop 1 outputs
              `shouldSatisfy` all ((== getDuration PT5) . getDuration)
        )
  it "only drops less than the static duration"
    $ property
    $ \ls ->
      withTestData
        ls
        PT5
        ( \inputs outputs ->
            ( sum (getDuration <$> inputs)
                - sum (getDuration <$> outputs)
            )
              `shouldSatisfy` (< getDuration PT5)
        )
  it "maintains strictly monotonic sequence numbers"
    $ property
    $ \ls ->
      withTestData
        ls
        PT5
        (const (`shouldSatisfy` seqNumStrictlyMonotoneIncreasing))
  it "maintains strictly monotonic timestamps"
    $ property
    $ \ls ->
      withTestData
        ls
        PT5
        (const (`shouldSatisfy` ticksStrictlyMonotoneIncreasing (nominalDiffTime # getStaticDuration PT5)))
