module Data.MediaBus.Conduit.SegmentSpec (spec) where

import Control.Lens
import Data.MediaBus
import Data.MediaBus.Conduit.SpecUtils
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "generates segments with a total duration equal to the input frames" $
    property $
      \ls (outputSegmentDuration :: Ticks64 (Hz 16000)) ->
        outputSegmentDuration > 0
          ==> runSegmetCOnTestData @(Hz 16000)
            ls
            (outputSegmentDuration ^. nominalDiffTime)
            ( \inputs outputs ->
                let inputSum, outputSum :: Ticks64 (Hz 16000)
                    inputSum = sum (map getDurationTicks inputs)
                    outputSum = sum (map getDurationTicks outputs)
                 in outputSum === inputSum
            )
  it "generates incomplete frames with previous left over input when it receives a start frame" $
    property $
      \ls1 ls2 (outputSegmentDuration :: Ticks64 (Hz 16000)) ->
        not (null ls1) && not (null ls2) && outputSegmentDuration > 0
          ==> let inputs = mkTestInputs @(Hz 16000) ls1 ++ mkTestInputs @(Hz 16000) ls2
                  outputs = runSegmetC inputs (outputSegmentDuration ^. nominalDiffTime)
                  inputSum, outputSum :: Ticks64 (Hz 16000)
                  inputSum = sum (map getDurationTicks inputs)
                  outputSum = sum (map getDurationTicks outputs)
               in outputSum === inputSum
  it "starts with the sequence numbers of the first frame" $
    property $
      \ls1 ls2 (outputSegmentDuration :: Ticks64 (Hz 16000)) ->
        not (null ls1) && not (null ls2) && outputSegmentDuration > 0
          ==> let inputs = mkTestInputs @(Hz 16000) ls1 ++ mkTestInputs @(Hz 16000) ls2
                  outputs = runSegmetC inputs (outputSegmentDuration ^. nominalDiffTime)
                  inputSum, outputSum :: Ticks64 (Hz 16000)
                  inputSum = sum (map getDurationTicks inputs)
                  outputSum = sum (map getDurationTicks outputs)
               in outputSum === inputSum

  it "generates no output if the output length is zero" $
    property $
      \ls ->
        runSegmetCOnTestData @(Hz 8000)
          ls
          0
          (\inputs outputs -> not (null inputs) ==> outputs === [])
  it "generates segments with the given suration, for the last one" $
    property $
      \ls ->
        runSegmetCOnTestData @(Hz 8000)
          ls
          0.005
          ( \_ outputs ->
              Prelude.drop 1 (Prelude.take (length outputs - 2) outputs)
                `shouldSatisfy` all ((== 0.005) . getDuration)
          )
  it "maintains strictly monotonic sequence numbers" $
    property $
      \ls ->
        runSegmetCOnTestData @(Hz 8000)
          ls
          0.005
          (const (`shouldSatisfy` seqNumStrictlyMonotoneIncreasing))
  it "maintains strictly monotonic timestamps" $
    property $
      \ls ->
        runSegmetCOnTestData @(Hz 8000)
          ls
          0.005
          (const (`shouldSatisfy` ticksStrictlyMonotoneIncreasing (nominalDiffTime # getStaticDuration PT5)))
