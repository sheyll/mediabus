module Data.MediaBus.Media.SyncStreamSpec
  ( spec,
  )
where

import Control.Lens
import Control.Monad.State
import Data.Function
import Data.MediaBus
import Debug.Trace
import Test.Hspec
import Test.QuickCheck
import FakePayload

spec :: Spec
spec =
  describe "setSequenceNumberAndTimestamp" $ do
    it "increases the sequence number by one (only) for each frame" $
      let prop :: (NonEmptyList (SyncStream () () FakePayload)) -> Bool
          prop (NonEmpty inStr) =
            let expectedLastSeqNum =
                  let isNext (MkStream (Next _)) = True
                      isNext _ = False
                   in max 0 (fromIntegral (length (filter isNext inStr)) - 1)
                actualLastSeqNum =
                  let outStr =
                        let z :: (SeqNum16, Ticks64At8000)
                            z = (0, 0)
                         in evalState
                              ( mapM
                                  (state . setSequenceNumberAndTimestamp)
                                  inStr
                              )
                              z
                   in case last outStr of
                        MkStream (Next f) -> f ^. seqNum
                        MkStream (Start f) -> max 0 (f ^. seqNum - 1)
             in expectedLastSeqNum == actualLastSeqNum
       in property prop
    it "increases the sequence number monotonic" $
      let prop :: (NonEmptyList (SyncStream () () FakePayload)) -> Bool
          prop (NonEmpty inStr) =
            let seqNumDiffs =
                  let outFrames =
                        let isNext (MkStream (Next _)) = True
                            isNext _ = False
                            outStr =
                              let z :: (SeqNum16, Ticks64At8000)
                                  z = (0, 0)
                               in evalState
                                    ( mapM
                                        (state . setSequenceNumberAndTimestamp)
                                        inStr
                                    )
                                    z
                         in filter isNext outStr
                   in zipWith ((-) `on` (view seqNum)) (drop 1 outFrames) outFrames
             in all (== 1) seqNumDiffs
       in property prop
    it "increases the timestamps by the duration of each frame" $
      let prop :: (NonEmptyList (SyncStream () () FakePayload)) -> Bool
          prop (NonEmpty inStr) =
            let isNext (MkStream (Next _)) = True
                isNext _ = False
                outFrames :: [Stream () SeqNum16 Ticks64At8000 () FakePayload]
                outFrames =
                  let outStr =
                        let z :: (SeqNum16, Ticks64At8000)
                            z = (0, MkTicks 0)
                         in evalState
                              ( mapM
                                  (state . setSequenceNumberAndTimestamp)
                                  inStr
                              )
                              z
                   in filter isNext outStr
                timestamps = map (view timestamp) outFrames
                expectedTimestamps :: [Ticks64At8000]
                expectedTimestamps =
                  let inFramesWithoutLast =
                        (filter isNext inStr)
                      inDurations =
                        map
                          (view (from nominalDiffTime) . getDuration)
                          inFramesWithoutLast
                   in scanl (+) 0 inDurations
             in if and (zipWith (==) timestamps expectedTimestamps)
                  then True
                  else
                    traceShow
                      ( timestamps,
                        expectedTimestamps,
                        outFrames
                      )
                      False
       in property prop
