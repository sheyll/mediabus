module Data.MediaBus.ReorderSpec ( spec ) where

import           Conduit
import           Data.Conduit.List
import           Data.MediaBus
import           Test.Hspec
import           Data.Proxy
import           Data.Word
import           Test.QuickCheck

spec :: Spec
spec = describe "reorderFramesBySeqNumC" $ do
    let runC inputs = runConduitPure (sourceList inputs .|
                                          annotateTypeC (Proxy :: Proxy (Stream () Word8 () () ()))
                                                         (reorderFramesBySeqNumC 3) .|
                                          consume)

    it "yields at least one 'Start' for each incoming 'Start'" $
        property $
            \inputs -> let countStarts = foldr (\x n -> case x of
                                                    MkStream (Start _) -> n + 1
                                                    MkStream (Next _) -> n)
                                               0
                       in
                           countStarts (runC inputs) `shouldSatisfy`
                               (>= countStarts inputs)
    it "yields exactly the given input if the input is ordered and gap-free" $
        let inputs = [ startFrame 0 ] ++
                [ nextFrame x
                | x <- [0 .. 10] ]
        in
            runC inputs `shouldBe`
                inputs
    it "reorders out of order packets (1)" $
        let inputs = [ startFrame 0
                     , nextFrame 0
                     , nextFrame 2
                     , nextFrame 3
                     , nextFrame 1
                     ]
            expected = [ startFrame 0
                       , nextFrame 0
                       , nextFrame 1
                       , nextFrame 2
                       , nextFrame 3
                       ]
        in
            runC inputs `shouldBe`
                expected
    it "reorders out of order packets (2)" $
        let inputs = [ startFrame 0, nextFrame 2, nextFrame 1, nextFrame 0 ]
            expected = [ startFrame 0, nextFrame 0, nextFrame 1, nextFrame 2 ]
        in
            runC inputs `shouldBe`
                expected
    it "skips over missing frames when the queue is full" $
        let inputs = [ startFrame 0
                     , nextFrame 0
                     , nextFrame 1
                     , nextFrame 3
                     , nextFrame 4
                     , nextFrame 5
                     ]
            expected = [ startFrame 0
                       , nextFrame 0
                       , nextFrame 1
                       , nextFrame 3
                       , nextFrame 4
                       , nextFrame 5
                       ]
        in
            runC inputs `shouldBe`
                expected
    it "restarts at the incoming sequence numbering after too many frame drops" $
        let inputs = [ startFrame 10
                     , nextFrame 10
                     , nextFrame 11
                     , nextFrame 12
                     , nextFrame 0
                     , nextFrame 1
                     , nextFrame 2
                     , nextFrame 3
                     , nextFrame 4
                     ]
            expected = [ startFrame 10
                       , nextFrame 10
                       , nextFrame 11
                       , nextFrame 12
                       , startFrame 2
                       , nextFrame 2
                       , nextFrame 3
                       , nextFrame 4
                       ]
        in
            runC inputs `shouldBe`
                expected
    it "restarts at the incoming sequence numbering after too many frame drops, AND flushes the queued elements first" $
        let inputs = [ startFrame 10
                     , nextFrame 13
                     , nextFrame 14
                     , nextFrame 0
                     , nextFrame 1
                     , nextFrame 2
                     , nextFrame 3
                     , nextFrame 4
                     ]
            expected = [ startFrame 10
                       , nextFrame 13
                       , nextFrame 14
                       , startFrame 2
                       , nextFrame 2
                       , nextFrame 3
                       , nextFrame 4
                       ]
        in
            runC inputs `shouldBe`
                expected
    it "flushes and resets its internal state after every 'Start'" $
        let inputs = [ startFrame 10
                     , nextFrame 13
                     , nextFrame 14
                     , startFrame 40
                     , nextFrame 40
                     , nextFrame 41
                     , startFrame 50
                     , nextFrame 0
                     , nextFrame 1
                     , startFrame 60
                     , nextFrame 50
                     , nextFrame 51
                     , nextFrame 52
                     ]
            expected = [ startFrame 10
                       , nextFrame 13
                       , nextFrame 14
                       , startFrame 40
                       , nextFrame 40
                       , nextFrame 41
                       , startFrame 50
                       , startFrame 60
                       , startFrame 52
                       , nextFrame 52
                       ]
        in
            runC inputs `shouldBe`
                expected
    it "yields monotone increasing frames higher than the start-frame" $
        property $
            \inputs -> runC inputs `shouldSatisfy`
                isMonoIncreasingAndHigherThanStartSeqNum

isMonoIncreasingAndHigherThanStartSeqNum :: [Stream () Word8 () () ()] -> Bool
isMonoIncreasingAndHigherThanStartSeqNum [] =
    True
isMonoIncreasingAndHigherThanStartSeqNum (MkStream (Start (MkFrameCtx () () sn ())) : rest) =
    isMonoIncreasingAndHigherThanStartSeqNumN sn rest
isMonoIncreasingAndHigherThanStartSeqNum rest@(MkStream (Next (MkFrame () sn ())) : _) =
    isMonoIncreasingAndHigherThanStartSeqNumN sn rest

isMonoIncreasingAndHigherThanStartSeqNumN _ [] =
    True
isMonoIncreasingAndHigherThanStartSeqNumN _ (MkStream (Start (MkFrameCtx () () sn ())) : rest) =
    isMonoIncreasingAndHigherThanStartSeqNumN sn rest
isMonoIncreasingAndHigherThanStartSeqNumN sn (MkStream (Next (MkFrame () snFrame ())) : rest)
    | snFrame >= sn = isMonoIncreasingAndHigherThanStartSeqNumN snFrame rest
    | otherwise = False

startFrame :: Word8 -> Stream () Word8 () () ()
startFrame sn = MkStream (Start (MkFrameCtx () () sn ()))

nextFrame :: Word8 -> Stream () Word8 () () ()
nextFrame sn = MkStream (Next (MkFrame () sn ()))
