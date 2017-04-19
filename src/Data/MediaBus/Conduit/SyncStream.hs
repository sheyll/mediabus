-- | 'Conduit's that remove or set the sequence numbers and time stamps in
--   'Stream's. The functions in this module lift the functions in 'SyncStream's
--   to conduits.
module Data.MediaBus.Conduit.SyncStream
  ( assumeSynchronizedC
  , setSequenceNumberAndTimestampC
  , convertTimestampC
  , setTimestampFromDurationsC
  , removeTimestampC
  ) where

import Control.Monad.State.Strict
import Control.DeepSeq (NFData)
import Data.Conduit
import Data.Conduit.Lift
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Stream
import Data.MediaBus.Media.SyncStream

-- * Combined Sequence number and timestamp calculation

-- | Assign sequence numbers and timestamps to the 'Frame's in a 'SyncStream',
-- starting both the sequence number and timestamp at @0@.
-- This functions is /strict/ and uses 'setSequenceNumberAndTimestamp'
-- under the hood.
-- Inorder to calculate only the 'timestamp' of a stream use
-- 'setTimestampFromDurationsC'.
setSequenceNumberAndTimestampC
  :: (Monad m, KnownRate r, HasDuration d, Num s, CanBeTicks r t)
  => ConduitM (SyncStream i p d) (Stream i s (Ticks r t) p d) m ()
setSequenceNumberAndTimestampC =
  evalStateC
    (0, 0)
    (awaitForever
       (\sIn -> do
          sOut <- lift (state (setSequenceNumberAndTimestamp sIn))
          yield sOut))

-- | Remove the sequence numbers and time stamps from a 'Stream'.
-- It's much more explicit to use a 'SyncStream' instead of a 'Stream'.
-- For example, when a library function aggregates 'Frame's but doesn't regard
-- the sequence numbers and time stamps, using 'SyncStream' indicates to users
-- of that library, that the function does not handle any gaps and/or out of order
-- packages or discrepancies in the time stamps and e.g. frame durations.
--
-- The user then knows, that she has to add functions to that conduit to accomodate
-- for that.
--
-- This functions is /strict/ and uses 'assumeSynchronized'
-- under the hood.
assumeSynchronizedC
  :: (Monad m, KnownRate r, HasDuration d, Num s, CanBeTicks r t)
  => ConduitM (SyncStream i p d) (Stream i s (Ticks r t) p d) m ()
assumeSynchronizedC =
  evalStateC
    (0, 0)
    (awaitForever
       (\sIn -> do
          sOut <- lift (state (setSequenceNumberAndTimestamp sIn))
          yield sOut))

-- * Timestamp calculation and conversion

-- | Set the timestamp of each element in the conduit.
-- The timestamp of each element is calculated from the sum of the durations of
-- the previous elements and the start time stamp @t0@.
--
-- The input elements must be instances of 'HasTimestamp' but with the important
-- condition, that the input timestamp is always /unit/ i.e. @()@.
-- This prevents /meaningful/ timestamps from being overwritten.
--
-- Use 'removeTimestampC' to explicitly remove a timestamp.
setTimestampFromDurationsC
  :: forall m r t a.
     ( Monad m
     , CanBeTicks r t
     , HasDuration a
     , HasTimestamp a
     , GetTimestamp a ~ ()
     )
  => Ticks r t -> Conduit a m (SetTimestamp a (Ticks r t))
setTimestampFromDurationsC t0 = evalStateC t0 (awaitForever go)
  where
    go !sb = lift (state (setTimestampFromDurations sb)) >>= yield

-- | Explicitly remove a timestamp, by setting the timestamp to @()@.
removeTimestampC
  :: (Monad m, HasTimestamp a)
  => Conduit a m (SetTimestamp a ())
removeTimestampC = awaitForever (yield . removeTimestamp)

-- | Recalculate all timestamps in a 'Stream'. This function is strict in its arguments.
convertTimestampC
  :: forall proxy0 proxy1 m r t r' t' i s c p.
     (NFData t, NFData t', CanBeTicks r t, CanBeTicks r' t', Monad m, NFData t')
  => proxy0 '( r, t)
  -> proxy1 '( r', t')
  -> Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r' t') p c)
convertTimestampC _ _ = mapTimestampC' convertTicks
