module Data.MediaBus.Stream.SegmentSpec ( spec ) where

import           Test.Hspec
import           Data.Conduit.List
import           Conduit
import           Test.QuickCheck
import           Data.MediaBus
import           Data.Word
import qualified Data.Vector.Storable as V
import           Control.Lens
import           Data.Proxy

spec :: Spec
spec = describe "segmentC" $ do
    it "only outputs segments with a valid length" $
        property $
            \ls -> withTestData ls
                                PT5
                                (\_ outputs -> Prelude.drop 1 outputs `shouldSatisfy`
                                     all ((== getDuration PT5) . getDuration))
    it "only drops less than the static duration" $
        property $
            \ls -> withTestData ls
                                PT5
                                (\inputs outputs -> (sum (getDuration <$> inputs) -
                                                         sum (getDuration <$> outputs)) `shouldSatisfy`
                                     (< getDuration PT5))
    it "maintains strictly monotonic sequence numbers" $
        property $
            \ls -> withTestData ls
                                PT5
                                (const (`shouldSatisfy` seqNumStrictlyMonotoneIncreasing))
    it "maintains strictly monotonic timestamps" $
        property $
            \ls -> withTestData ls
                                PT5
                                (const (`shouldSatisfy` ticksStrictlyMonotoneIncreasing (nominalDiffTime # getStaticDuration PT5)))

withTestData :: HasStaticDuration d
             => [TestLen]
             -> PT d
             -> ([Stream () Word8 (Ticks (Hz 8000) Word32) () (Audio (Hz 8000) Mono (Raw S16))]
                 -> [Stream () Word8 (Ticks (Hz 8000) Word32) () (Segment d (Audio (Hz 8000) Mono (Raw S16)))]
                 -> res)
             -> res
withTestData ls pt f = let inputs = mkTestInputs ls
                           outputs = runSegmentC inputs pt
                       in
                           f inputs outputs

seqNumStrictlyMonotoneIncreasing outs =
    let res = view seqNum <$> outs
    in
        all (== 1) $ zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res)

ticksStrictlyMonotoneIncreasing dur outs =
    let res = view timestamp' <$> outs
    in
        all (== dur) $ zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res)

runSegmentC :: (HasStaticDuration d, HasDuration c, CanSegment c, Monoid c)
            => [Stream () Word8 (Ticks (Hz 8000) Word32) () c]
            -> PT d
            -> [Stream () Word8 (Ticks (Hz 8000) Word32) () (Segment d c)]
runSegmentC inputs _p = runConduitPure (sourceList inputs .|
                                            segmentC .|
                                            consume)

mkTestInputs :: [TestLen]
             -> [Stream () Word8 (Ticks (Hz 8000) Word32) () (Audio (Hz 8000) Mono (Raw S16))]
mkTestInputs = reverse .
    snd .
        foldl (\((ts0, sn0), acc0) (MkTestLen len) ->
                   ( (ts0 + fromIntegral len, sn0 + 1)
                   , mkTestPacket sn0 ts0 len : acc0
                   ))
              ((0, 0), [ mkTestStartPacket ])
  where
    mkTestPacket sn ts len =
        MkStream (Next (MkFrame ts sn (pcmMediaBuffer . mediaBufferVector # (V.replicate len 0))))

    mkTestStartPacket = MkStream (Start (MkFrameCtx () 0 0 ()))

data PT d where
        PT0 :: PT (0 :/ Hz 1)
        PT5 :: PT (5 :/ Hz 1000)
        PT10 :: PT (80 :/ Hz 8000)
        PT20 :: PT (320 :/ Hz 16000)

instance HasStaticDuration d =>
         HasDuration (PT d) where
    getDuration _ = getStaticDuration (Proxy :: Proxy d)

newtype TestLen = MkTestLen Int
    deriving (Read, Show, Ord, Eq, Num)

instance Arbitrary TestLen where
    arbitrary = MkTestLen <$> choose (0, 1000)
