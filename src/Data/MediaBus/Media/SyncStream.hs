-- | A module for removing and creating sequence numbers and time stamps for
-- 'Stream's.
--
-- It's sometimes helpful to explicity use a 'SyncStream' instead of a 'Stream'.
--
-- For example, for a library function that consumes 'Frame's and doesn't regard
-- the sequence numbers and time stamps, such that the function does not handle
-- any gaps and/or out of order packages or discrepancies in the time stamps
-- and frame durations.
--
-- When the library author chooses 'SyncStream', the library users then know,
-- that the library function relies on a synchronized stream.
module Data.MediaBus.Media.SyncStream
  ( SyncStream,
    assumeSynchronized,
    setSequenceNumberAndTimestamp,
  )
where

import Data.MediaBus.Basics.Series (Series (Next, Start))
import Data.MediaBus.Basics.Ticks
  ( CanBeTicks,
    HasDuration (getDurationTicks),
    Ticks,
  )
import Data.MediaBus.Media.Stream
  ( Frame (MkFrame),
    FrameCtx (MkFrameCtx),
    Stream (MkStream),
  )

-- | A 'Stream' without sequence numbers and time stamps is called a
-- 'SyncStream', which is the abbreviation of /synchronous stream/, because
-- when the sequence numbers and time stamps of a 'Stream', and by extension of
-- a 'Frame' and 'FrameCtx', are always @()@, the 'Frame's of a 'Stream' can
-- be assumed to be (perfectly) synchronous.
type SyncStream streamId streamStartPayload payload =
  Stream streamId () () streamStartPayload payload

-- | Convert a 'Stream' to a 'SyncStream' by simply /forgetting/ the sequence
-- numbers and timestamps of the input. This expresses the assumption that the
-- 'Frame's are either perfectly lined sequential or that this doesn't matter
-- at all.
assumeSynchronized ::
  Stream streamId s t streamStartPayload payload ->
  SyncStream streamId streamStartPayload payload
assumeSynchronized (MkStream (Start (MkFrameCtx i _ _ p))) =
  MkStream (Start (MkFrameCtx i () () p))
assumeSynchronized (MkStream (Next (MkFrame _ _ payload))) =
  MkStream (Next (MkFrame () () payload))

-- | Set sequence numbers and timestamps.
-- Increment the sequence numbers starting from @0@ for every frame.
-- Start the timestamp at @0@ and add the 'Frame' duration of the 'Next'
-- frame in the stream.
-- This function has the signature required to turn it into a 'State' monad.
setSequenceNumberAndTimestamp ::
  (Num sequenceNumber, CanBeTicks rate timestamp, HasDuration payload) =>
  SyncStream streamId streamStartPayload payload ->
  (sequenceNumber, Ticks rate timestamp) ->
  ( Stream
      streamId
      sequenceNumber
      (Ticks rate timestamp)
      streamStartPayload
      payload,
    (sequenceNumber, Ticks rate timestamp)
  )
setSequenceNumberAndTimestamp (MkStream (Next (MkFrame _t _s !c))) (nextS, nextT) =
  ( MkStream (Next (MkFrame nextT nextS c)),
    (nextS + 1, nextT + getDurationTicks c)
  )
setSequenceNumberAndTimestamp (MkStream (Start (MkFrameCtx i _t _s p))) (nextS, nextT) =
  (MkStream (Start (MkFrameCtx i nextT nextS p)), (nextS, nextT))
