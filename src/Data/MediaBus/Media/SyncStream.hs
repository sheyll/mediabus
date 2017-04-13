-- | Naturally ordered 'Stream's.
-- A 'Stream' without sequence numbers and timestamps has no means to represent
-- 'Frame's that have varying sequence numbers or timestamps, hence they cannot
-- be part of a stream that is not perfectly synchronized, therefore when consuming
-- such 'Stream' values, the corresponding callers must work under the assumption
-- that the frames are perfectly synchronous.
module Data.MediaBus.Media.SyncStream
  ( type SyncStream
  , assumeSynchronized
  , setSequenceNumbersAndTimestamps
  ) where

import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Stream

-- | A 'Stream' without a meaningful sequence number or timestamp.
type SyncStream i p c = Stream i () () p c

-- | Convert a 'Stream' to a 'SyncStream' by simply /forgetting/ the sequence
-- numbers and timestamps of the input. This expresses the assumption that the
-- 'Frame's are either perfectly lined sequential or that this doesn't matter
-- at all.
assumeSynchronized :: Stream i s t p c -> SyncStream i p c
assumeSynchronized (MkStream (Start (MkFrameCtx i _ _ p))) =
  MkStream (Start (MkFrameCtx i () () p))
assumeSynchronized (MkStream (Next (MkFrame _ _ c))) =
  MkStream (Next (MkFrame () () c))

-- | Set sequence numbers and timestamps.
-- Increment the sequence numbers starting from @0@ for every frame.
-- Start the timestamp at @0@ and add the 'Frame' duration of the 'Next'
-- frame in the stream.
-- This function has the signature required to turn it into a 'State' monad
setSequenceNumbersAndTimestamps
  :: (Num s, CanBeTicks r t, HasDuration c)
  => SyncStream i p c
  -> (s, Ticks r t)
  -> (Stream i s (Ticks r t) p c, (s, Ticks r t))
setSequenceNumbersAndTimestamps (MkStream (Next (MkFrame _t _s !c))) (nextS, nextT) =
  ( MkStream (Next (MkFrame nextT nextS c))
  , (nextS + 1, nextT + getDurationTicks c))
setSequenceNumbersAndTimestamps (MkStream (Start (MkFrameCtx i _t _s p))) (nextS, nextT) =
  ((MkStream (Start (MkFrameCtx i nextT nextS p))), (nextS, nextT))
