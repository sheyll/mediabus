{-# LANGUAGE NumericUnderscores #-}

module Data.MediaBus.Conduit.AggregateSpec (spec) where

import Conduit
import Control.Lens
import Data.Conduit.List hiding (map)
import Data.MediaBus
import Data.MediaBus.Conduit.SpecUtils
import Data.Word (Word32)
import Test.Hspec
import Test.QuickCheck

-- Group content frames into a list of frames such that the total duration
-- of the contents is just @>= t@.
--
-- If you want a specific number of contents, and the contents have no 'HasDuration'
-- instance, use 'aggregateCountC'.
--
-- The sequence numbers are ignored.
--
-- The timestamps are taken from the the first content of each aggregate.
--
-- When a start frame is received, the current aggregate is sent,
-- even if it is incomplete, and the start frame is passed through.

spec :: Spec
spec = do
  describe "aggregateCountC" $ do
    it "basically works" $
      let testInputs :: [Stream Int Word32 (Ticks (Hz 1) Word32) (Maybe String) (PT (1 :/ Hz 1))]
          testInputs =
            fmap
              MkStream
              [ Start (MkFrameCtx 7443 1000 200 (Just "start")),
                Next (MkFrame 1000 200 PT1),
                Next (MkFrame 1001 201 PT1),
                Next (MkFrame 1002 202 PT1),
                Next (MkFrame 1003 666 PT1),
                Next (MkFrame 1004 2341241 PT1),
                Next (MkFrame 1005 205 PT1),
                Next (MkFrame 7777 206 PT1),
                Next (MkFrame 7777 207 PT1),
                Next (MkFrame 7777 208 PT1)
              ]
          aggregationDuration = 3
          outputs =
            runConduitPure
              ( sourceList testInputs
                  .| aggregateCountC aggregationDuration
                  .| consume
              )
       in outputs
            `shouldBe` fmap
              MkStream
              [ Start (MkFrameCtx 7443 () () (Just "start")),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 1000 200 PT1,
                            MkFrame 1001 201 PT1,
                            MkFrame 1002 202 PT1
                          ]
                      )
                  ),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 1003 666 PT1,
                            MkFrame 1004 2341241 PT1,
                            MkFrame 1005 205 PT1
                          ]
                      )
                  ),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 7777 206 PT1,
                            MkFrame 7777 207 PT1,
                            MkFrame 7_777 208 PT1
                          ]
                      )
                  )
              ]
    it "flushes the accumulated buffers when a start frame arives" $
      let testInputs :: [Stream Int Word32 (Ticks (Hz 1) Word32) (Maybe String) (PT (1 :/ Hz 1))]
          testInputs =
            fmap
              MkStream
              [ Start (MkFrameCtx 7443 1000 200 (Just "start1")),
                Next (MkFrame 1000 200 PT1),
                Next (MkFrame 1001 201 PT1),
                Start (MkFrameCtx 4516 15200 20 (Just "start2")),
                Next (MkFrame 1002 202 PT1),
                Start (MkFrameCtx 4516 15200 20 (Just "start3")),
                Start (MkFrameCtx 43516 1200 56 (Just "start4")),
                Next (MkFrame 1003 666 PT1),
                Next (MkFrame 1004 2341241 PT1),
                Next (MkFrame 1005 205 PT1),
                Next (MkFrame 7777 206 PT1),
                Next (MkFrame 7777 207 PT1)
              ]
          aggregationDuration = 3
          outputs =
            runConduitPure
              ( sourceList testInputs
                  .| aggregateCountC aggregationDuration
                  .| consume
              )
       in outputs
            `shouldBe` fmap
              MkStream
              [ Start (MkFrameCtx 7443 () () (Just "start1")),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 1000 200 PT1,
                            MkFrame 1001 201 PT1
                          ]
                      )
                  ),
                Start (MkFrameCtx 4516 () () (Just "start2")),
                Next (MkFrame () () (MkFrames [MkFrame 1002 202 PT1])),
                Start (MkFrameCtx 4516 () () (Just "start3")),
                Start (MkFrameCtx 43516 () () (Just "start4")),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 1003 666 PT1,
                            MkFrame 1004 2341241 PT1,
                            MkFrame 1005 205 PT1
                          ]
                      )
                  ),
                Next
                  ( MkFrame
                      ()
                      ()
                      ( MkFrames
                          [ MkFrame 7777 206 PT1,
                            MkFrame 7777 207 PT1
                          ]
                      )
                  )
              ]
    it "when aggregating n frames, it generates k output frames for (n * k) input frames" $
      property $ \(Positive n) (Positive k) ->
        n > 0 && k > 0
          ==> let inputs =
                    Prelude.replicate
                      (n * k)
                      (MkStream (Next (MkFrame () () ())))

                  outputs =
                    runConduitPure
                      ( sourceList inputs
                          .| aggregateCountC (fromIntegral n)
                          .| consume
                      )
               in length outputs === k

    it "when aggregating n frames, it generates k+1 output frames for (n * k + m) input frames, where 0 < m < n" $
      property $ \(Positive n) (Positive k) -> do
        m <- choose (0, n)
        return $
          (m > 0 && m < n && n > 0 && k > 0)
            ==> let inputs =
                      Prelude.replicate
                        (n * k + m)
                        (MkStream (Next (MkFrame () () ())))

                    outputs =
                      runConduitPure
                        ( sourceList inputs
                            .| aggregateCountC (fromIntegral n)
                            .| consume
                        )
                 in length outputs === k + 1

  describe "aggregateDurationC" $ do
    it "when the aggregation duration is n times the the duration of each input frame, it generates k output frames for (n * k) input frames" $
      property $ \(Small inputDuration) (Positive n) (Positive k) ->
        n > 0 && k > 0 && inputDuration > 0
          ==> let inputs =
                    mkTestInputs @(Hz 16000)
                      ( Prelude.replicate
                          (n * k)
                          (MkTestLen (fromIntegral inputDuration))
                      )
                  aggregationDuration =
                    view
                      nominalDiffTime
                      (fromIntegral n * mkTicks32At16000 inputDuration)
                  outputs =
                    runConduitPure
                      ( sourceList inputs
                          .| aggregateDurationC aggregationDuration
                          .| consume
                      )
               in length (toListOf (each . eachFramePayload) outputs) === k
    it "the output durations average the input durations" $
      let testInputs :: [Stream () () () () (Ticks32 (Hz 1))]
          threeSecondsFrame = MkFrame () () (MkTicks 3)
          threeSecondsMedia = MkStream (Next threeSecondsFrame)
          testInputs = Prelude.replicate 10 threeSecondsMedia
          aggregationDuration = 10
          outputs =
            runConduitPure
              ( sourceList testInputs
                  .| aggregateDurationC aggregationDuration
                  .| consume
              )
       in outputs
            `shouldBe` fmap
              MkStream
              [ Next (MkFrame () () (MkFrames [threeSecondsFrame, threeSecondsFrame, threeSecondsFrame, threeSecondsFrame])), -- 12
                Next (MkFrame () () (MkFrames [threeSecondsFrame, threeSecondsFrame, threeSecondsFrame])), --21
                Next (MkFrame () () (MkFrames [threeSecondsFrame, threeSecondsFrame, threeSecondsFrame]))  --30
              ]

    it "groups frames into the given duration, as good as possible" $
      property $ \(aggregationDuration :: Ticks32At16000) inputLengths ->
        let inputs = mkTestInputs @(Hz 16000) inputLengths
            outputs =
              runConduitPure
                ( sourceList inputs
                    .| aggregateDurationC aggregateDurationSecs
                    .| consume
                )
            aggregateDurationSecs = view nominalDiffTime aggregationDuration
            totalOutputDuration =
              sumOf
                (each . eachFramePayload . folded . to getDuration)
                outputs
            totalInputDuration = sum (map getDuration inputs)
            maxInputDuration = maximum (map getDuration inputs)
         in aggregationDuration > 0
              ==> if null inputs
                then totalOutputDuration === 0
                else
                  classify
                    (totalInputDuration < aggregateDurationSecs)
                    "less input than aggregation duration"
                    (totalInputDuration === totalOutputDuration)
                    .&&. classify
                      (totalInputDuration >= aggregateDurationSecs)
                      "more input than aggregation duration"
                      (totalInputDuration === totalOutputDuration)
    it "flushes the accumulated buffers when a start frame arives" $
      let testInputs0 =
            mkTestInputs @(Hz 1) [MkTestLen 1, MkTestLen 1]
          testInputs1 =
            mkTestInputs @(Hz 1) [MkTestLen 1, MkTestLen 1, MkTestLen 1, MkTestLen 1, MkTestLen 1]
          aggregationDuration = MkTicks @(Hz 1) @Word32 3
          outputs =
            runConduitPure
              ( sourceList (testInputs0 ++ testInputs1)
                  .| aggregateDurationC (aggregationDuration ^. nominalDiffTime)
                  .| consume
              )
       in outputs
            `shouldBe` fmap
              MkStream
              [ Start (MkFrameCtx () () () ()),
                Next (MkFrame () () (MkFrames $ takePayloadFrames testInputs0)),
                Start (MkFrameCtx () () () ()),
                Next (MkFrame () () (MkFrames $ Prelude.take 3 (takePayloadFrames testInputs1))),
                Next (MkFrame () () (MkFrames $ Prelude.drop 3 (takePayloadFrames testInputs1)))
              ]
