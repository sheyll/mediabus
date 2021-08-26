module Data.MediaBus.Conduit.StreamSpec (spec) where

import Conduit
import Control.Lens
import Control.Monad
import Data.Conduit.List
import Data.MediaBus
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "Stream conduits"
    $ do
        describe "Functor instance: fmap"
              $ it "should have the same result as 'overFrameContentC (...) f'" pending
        describe "assumeSynchronized" $
          it "removes sequence numbers and timestamps but passes all buffers untouched" $
            property $ \(inputs :: [Stream Bool Int Int Int Int]) ->
              let outputs = runConduitPure (yieldMany inputs .| assumeSynchronizedC .| sinkList )
              in
                length inputs === length outputs
                .&&.
                toListOf (each . eachFramePayload) inputs
                 === toListOf (each . eachFramePayload) outputs

-- fmap f = overFrameContentC undefined (mapInput _framePayload (const Nothing) (const f))
_helloWorld :: IO ()
_helloWorld =
  void $
    runConduit
      ( yieldMany ("Hello world" :: String)
          .| traceShowC 1 "YO"
          .| consume
      )

_sampleSomeStream :: IO ALittleOutOfOrder
_sampleSomeStream = MkALittleOutOfOrder <$> sample' arbitrary

_yieldStream ::
  Monad m =>
  ALittleOutOfOrder ->
  ConduitT () (Stream () (SeqNum Word16) () () ()) m ()
_yieldStream (MkALittleOutOfOrder someFrames) =
  yieldMany someFrames
    .| traceShowC 1 "ORIGINAL"

_reorderSomeFrames =
  void $
    _sampleSomeStream
      >>= ( \fs ->
              runConduit
                ( _yieldStream fs
                    .| reorderFramesBySeqNumC 2
                    .| traceShowC 1 "     ORDERED"
                    .| consume
                )
          )

newtype ALittleOutOfOrder
  = MkALittleOutOfOrder
      { fromALittleOutOfOrder :: [Stream () (SeqNum Word16) () () ()]
      }

instance Arbitrary ALittleOutOfOrder where
  arbitrary = do
    start <- MkStream . Start <$> arbitrary
    (len :: Int) <- arbitrary
    MkALittleOutOfOrder <$> loop (start ^. seqNum) (len + 10) [start]
    where
      loop (MkSeqNum lastSeq) n acc
        | n == 0 = return acc
        | otherwise = do
          nextSeq' <- choose (lastSeq + 1, lastSeq + 2)
          ts <- arbitrary
          c <- arbitrary
          let nextSeq = MkSeqNum nextSeq'
          loop
            nextSeq
            (n - 1)
            (MkStream (Next (MkFrame ts nextSeq c)) : acc)
