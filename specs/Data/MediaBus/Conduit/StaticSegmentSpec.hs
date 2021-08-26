module Data.MediaBus.Conduit.StaticSegmentSpec (spec) where

import Control.Lens
import Data.MediaBus
import Data.MediaBus.Conduit.SpecUtils
import Test.Hspec
import Test.QuickCheck
import Data.Word (Word32)

spec :: Spec
spec = do
  it "sum of the durations of the generated segments is equal to the sum of the input frames rounded to the next multiple of the target duration"
    $ property
    $ \ls ->
      let
        outputPayload = PT1024
        outputSegmentDuration :: Ticks (Hz 16000) Word32
        outputSegmentDuration = getDurationTicks outputPayload
      in runStaticSegmetCOnTestData @(Hz 16000)
        ls
        outputPayload
        ( \inputs outputs ->
            let inputSum = sum (map getDurationTicks inputs)
                outputSum = sum (map getDurationTicks outputs)
            in ((inputSum `div` outputSegmentDuration) * outputSegmentDuration)
                  === outputSum .||.
                    inputSum === outputSum -- for debugging
        )
  it "if the output length is zero, no output is generated"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData  @(Hz 8000)
        ls
        PT0
        ( \inputs outputs -> not (null inputs) ==> outputs === [] )
  it "only outputs segments with a valid length"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData  @(Hz 8000)
        ls
        PT5
        ( \_ outputs ->
            Prelude.drop 1 outputs
              `shouldSatisfy` all ((== getDuration PT5) . getDuration)
        )
  it "only outputs segments with a valid length"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData  @(Hz 8000)
        ls
        PT20
        ( \_ outputs ->
            Prelude.drop 1 outputs
              `shouldSatisfy` all ((== getDuration PT20) . getDuration)
        )
  it "only outputs segments with a valid length"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData  @(Hz 8000)
        ls
        PT1024
        ( \_ outputs ->
            Prelude.drop 1 outputs
              `shouldSatisfy` all ((== getDuration PT1024) . getDuration)
        )
  it "only drops less than the static duration"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData @(Hz 8000)
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
      runStaticSegmetCOnTestData @(Hz 8000)
        ls
        PT5
        (const (`shouldSatisfy` seqNumStrictlyMonotoneIncreasing))
  it "maintains strictly monotonic timestamps"
    $ property
    $ \ls ->
      runStaticSegmetCOnTestData @(Hz 8000)
        ls
        PT5
        (const (`shouldSatisfy` ticksStrictlyMonotoneIncreasing (nominalDiffTime # getStaticDuration PT5)))
