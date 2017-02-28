module Data.MediaBus.StreamSpec ( spec ) where

import           Test.Hspec
import           Test.QuickCheck
import           Conduit
import           Data.Conduit.List
import           Data.MediaBus
import           Control.Monad
import           Data.Word
import           Control.Lens

spec :: Spec
spec = describe "Stream conduits" $
    describe "Functor instance: fmap" $
        it "should have the same result as 'overFrameContentC (...) f'" pending

-- fmap f = overFrameContentC undefined (mapInput _framePayload (const Nothing) (const f))
_helloWorld :: IO ()
_helloWorld = void $
    runConduit (yieldMany ("Hello world" :: String) .|
                    traceShowC 1 "YO" .|
                    consume)

_sampleSomeStream :: IO ALittleOutOfOrder
_sampleSomeStream = MkALittleOutOfOrder <$> sample' arbitrary

_yieldStream :: Monad m
             => ALittleOutOfOrder
             -> Source m (Stream () (SeqNum Word16) () () ())
_yieldStream (MkALittleOutOfOrder frames) =
    yieldMany frames .|
        traceShowC 1 "ORIGINAL"

_reorderSomeFrames = void $
    _sampleSomeStream >>=
        (\fs -> runConduit (_yieldStream fs .|
                                reorderFramesBySeqNumC 2 .|
                                traceShowC 1 "     ORDERED" .|
                                consume))

newtype ALittleOutOfOrder =
      MkALittleOutOfOrder { fromALittleOutOfOrder :: [Stream () (SeqNum Word16) () () ()]
                          }

instance Arbitrary ALittleOutOfOrder where
    arbitrary = do
        start <- (MkStream . Start) <$> arbitrary
        (len :: Int) <- arbitrary
        MkALittleOutOfOrder <$> loop (start ^. seqNum) (len + 10) [ start ]
      where
        loop (MkSeqNum lastSeq) n acc
            | n == 0 = return acc
            | otherwise = do
                  nextSeq' <- choose (lastSeq + 1, lastSeq + 2)
                  ts <- arbitrary
                  c <- arbitrary
                  let nextSeq = MkSeqNum nextSeq'
                  loop nextSeq
                       (n - 1)
                       (MkStream (Next (MkFrame ts nextSeq c)) : acc)
