module Data.MediaBus.Conduit.SpecUtils where

import Conduit
import Control.Lens
import Data.Conduit.List
import Data.MediaBus
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Word
import Test.QuickCheck
import Data.Time (NominalDiffTime)

runStaticSegmetCOnTestData ::
  (KnownRate r, HasStaticDuration d) =>
  [TestLen] ->
  PT d ->
  ( [Stream () Word8 (Ticks r Word32) () (Audio r Mono (Raw S16))] ->
    [Stream () Word8 (Ticks r Word32) () (StaticSegment d (Audio r Mono (Raw S16)))] ->
    res
  ) ->
  res
runStaticSegmetCOnTestData ls pt f =
  let inputs = mkTestInputs ls
      outputs = runStaticSegmetC inputs pt
   in f inputs outputs

runSegmetCOnTestData ::
  (KnownRate r) =>
  [TestLen] ->
  NominalDiffTime ->
  ( [Stream () Word8 (Ticks r Word32) () (Audio r Mono (Raw S16))] ->
    [Stream () Word8 (Ticks r Word32) () (Segment (Audio r Mono (Raw S16)))] ->
    res
  ) ->
  res
runSegmetCOnTestData ls pt f =
  let inputs = mkTestInputs ls
      outputs = runSegmetC inputs pt
   in f inputs outputs

seqNumStrictlyMonotoneIncreasing :: (HasSeqNum t, Eq (GetSeqNum t), Num (GetSeqNum t)) => [t] -> Bool
seqNumStrictlyMonotoneIncreasing outs =
  let res = view seqNum <$> outs
   in all (== 1) $ zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res)

ticksStrictlyMonotoneIncreasing ::
  (HasTimestamp s, Eq (GetTimestamp s), Num (GetTimestamp s)) =>
  GetTimestamp s ->
  [s] ->
  Bool
ticksStrictlyMonotoneIncreasing dur outs =
  let res = view timestamp' <$> outs
   in all (== dur) $ zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res)

runStaticSegmetC ::
  (HasStaticDuration d, HasDuration c, CanSegment c, Monoid c, KnownRate r) =>
  [Stream () Word8 (Ticks r Word32) () c] ->
  PT d ->
  [Stream () Word8 (Ticks r Word32) () (StaticSegment d c)]
runStaticSegmetC inputs _p =
  runConduitPure
    ( sourceList inputs
        .| staticSegmentC
        .| consume
    )

runSegmetC ::
  ( HasDuration c, CanSegment c, Monoid c, KnownRate r) =>
  [Stream () Word8 (Ticks r Word32) () c] ->
  NominalDiffTime ->
  [Stream () Word8 (Ticks r Word32) () (Segment c)]
runSegmetC inputs p =
  runConduitPure
    ( sourceList inputs
        .| segmentC p
        .| consume
    )

mkTestInputs ::
  KnownRate r =>
  [TestLen] ->
  [Stream () Word8 (Ticks r Word32) () (Audio r Mono (Raw S16))]
mkTestInputs =
  reverse
    . snd
    . foldl
      ( \((ts0, sn0), acc0) (MkTestLen len) ->
          ( (ts0 + fromIntegral len, sn0 + 1),
            mkTestPacket sn0 ts0 len : acc0
          )
      )
      ((0, 0), [mkTestStartPacket])
  where
    mkTestPacket sn ts len =
      MkStream
        ( Next
            ( MkFrame
                ts
                sn
                (rawPcmAudioBuffer . mediaBufferVector # V.replicate len 0xD5)
            )
        )
    mkTestStartPacket = MkStream (Start (MkFrameCtx () 0 0 ()))

data PT d where
  PT0 :: PT (0 :/ Hz 1)
  PT1 :: PT (1 :/ Hz 1)
  PT5 :: PT (5 :/ Hz 1000)
  PT10 :: PT (80 :/ Hz 8000)
  PT20 :: PT (320 :/ Hz 16000)
  PT1024 :: PT (16384 :/ Hz 16000)
  PT2048 :: PT (32768 :/ Hz 16000)

instance Show (PT d) where
  show PT0 = "PT0"
  show PT1 = "PT1"
  show PT5 = "PT5"
  show PT10 = "PT10"
  show PT20 = "PT20"
  show PT1024 = "PT1024"
  show PT2048 = "PT2048"

instance Eq (PT d) where
  PT0 == PT0 = True
  PT1 == PT1 = True
  PT5 == PT5 = True
  PT10 == PT10 = True
  PT20 == PT20 = True
  PT1024 == PT1024 = True
  PT2048 == PT2048 = True

instance
  HasStaticDuration d =>
  HasDuration (PT d)
  where
  getDuration _ = getStaticDuration (Proxy :: Proxy d)

newtype TestLen = MkTestLen Int
  deriving (Read, Show, Ord, Eq, Num)

instance Arbitrary TestLen where
  arbitrary = MkTestLen <$> choose (0, 1000)
