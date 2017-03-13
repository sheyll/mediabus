-- | This module contains functions and types for reframing the sequence number
--   and timestamps of 'Frame's.
--
--   This means that this module will allow you to record a set of incoming
--   frames using 'pushFrame'. Whenever you want to extract the timing and
--   sequence number information of the next frame with a given duration,
--   use 'generateFrame'.
module Data.MediaBus.Media.Reframe
  ( initialReframerState
  , runReframer
  , pushStartFrame
  , pushFrame
  , PushFrameError(..)
  , nextFrameAvailableDuration
  , nextFrameTimestamp
  , generateFrame
  , type ReframerT
  , ReframerSt()
  , ReframeError(..)
  , mkReFrameError
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Strict as State
import Data.MediaBus.Media.Stream
import Data.Typeable

-- | Create an empty initial state.
initialReframerState
  :: (Num d, Num s)
  => ReframerSt s d
initialReframerState = MkTimeFrame 0 0 0

-- | Run state 'ReframerSt' state transformer.
runReframer
  :: Monad m
  => ReframerT m s d a -> ReframerSt s d -> m (a, ReframerSt s d)
runReframer = runStateT

-- | Reset the current timing and sequence number, and start with the given
--   start time.
pushStartFrame
  :: (Num d, Monad m)
  => d -> ReframerT m s d ()
pushStartFrame d = do
  (MkTimeFrame _ s _) <- State.get
  State.put (MkTimeFrame d s d)

-- | Increase the available duration by the duration in the frame,  iff the
--   timestamp of the given frame matches exactly the timestamp after the end
--   of the available period, otherwise do nothing with the state and return
--   'True'.
pushFrame
  :: (Num d, Monad m, Eq d, Ord d)
  => Frame s d d -> ReframerT m s d (Maybe PushFrameError)
pushFrame (MkFrame ts _ dur) = do
  (MkTimeFrame startTs nextSeq endTs) <- State.get
  let endTs' = ts + dur
  when (ts == endTs) (State.put (MkTimeFrame startTs nextSeq endTs'))
  return
    (if | ts < endTs && endTs' < endTs -> Just InputFrameIsLate
        | ts < endTs && endTs' >= endTs -> Just InputFrameOverlaps
        | ts > endTs -> Just InputFrameIsEarly
        | ts == endTs -> Nothing)

-- | Specifies in what way 'pushFrame' failed
data PushFrameError
  = InputFrameIsLate -- ^ The input frame ends before '_endTs'
  | InputFrameOverlaps -- ^ The input frame starts before, and ends after '_endTs'
  | InputFrameIsEarly -- ^ The input frame begins after '_endTs'
  deriving (Eq, Ord, Show, Enum)

-- | Return the duration of the frames recorded with 'pushFrame'.
nextFrameAvailableDuration
  :: (Num d, Monad m)
  => ReframerT m s d d
nextFrameAvailableDuration = do
  (MkTimeFrame startTs _ endTs) <- State.get
  return (endTs - startTs)

-- | Return the timestamp of the frame being build. .
nextFrameTimestamp
  :: (Num d, Monad m)
  => ReframerT m s d d
nextFrameTimestamp = do
  (MkTimeFrame startTs _ _) <- State.get
  return startTs

-- | Try to create a frame with the given duration, and update the state
--   accordingly, the actual duration, that was available is
--   put into the payload field of the frame returned.
--   The start time stamp of the next frame
--   is always incremented by the @wantedDureation@ regardless of wether it was
--   available.
generateFrame
  :: (Num s, Num d, Monad m, Eq d, Ord d, Show d)
  => d -> ReframerT m s d (Frame s d d)
generateFrame wantedDuration = do
  (MkTimeFrame startTs nextSeq endTs) <- State.get
  available <- nextFrameAvailableDuration
  let endTs' = max (startTs + wantedDuration) endTs
  State.put (MkTimeFrame (startTs + wantedDuration) (nextSeq + 1) endTs')
  return (MkFrame startTs nextSeq (wantedDuration `min` available))

-- | Reframer state.
data ReframerSt s d = MkTimeFrame
  { _startTs :: !d
  , _nextSeqNum :: !s
  , _endTs :: !d
  } deriving (Typeable)

instance (Show s, Show d) =>
         Show (ReframerSt s d) where
  showsPrec d MkTimeFrame {_startTs, _nextSeqNum, _endTs} =
    showParen
      (d > 10)
      (showString "reframer-state: " .
       showString "start: " .
       showsPrec 11 _startTs .
       showString ", end: " .
       showsPrec 11 _endTs . showString ", next-sn: " . showsPrec 11 _nextSeqNum)

-- | The 'ReframerSt' 'StateT' transformer
type ReframerT m s d a = StateT (ReframerSt s d) m a

-- | The exception type for 'encodeLinearToAacC'
data ReframeError s d = MkReframeError
  { reframeError :: String
  , reframeErrorRequestedOutput :: Maybe d
  , reframeErrorSt :: ReframerSt s d
  } deriving (Typeable)

instance (Show s, Show d) =>
         Show (ReframeError s d) where
  showsPrec d MkReframeError { reframeError
                             , reframeErrorRequestedOutput
                             , reframeErrorSt
                             } =
    showParen
      (d > 10)
      (showString "reframe-error: " .
       showsPrec 11 reframeError .
       maybe
         id
         (\ro -> showString ", requested: " . showsPrec 11 ro)
         reframeErrorRequestedOutput .
       showString ", " . showsPrec 11 reframeErrorSt)

instance (Show s, Typeable s, Show d, Typeable d) =>
         Exception (ReframeError s d)

-- | Utility function to generate a 'ReframeError' with the current state.
mkReFrameError
  :: Monad m
  => String -> Maybe d -> ReframerT m s d (ReframeError s d)
mkReFrameError msg requestedOutputDuration =
  MkReframeError msg requestedOutputDuration <$> State.get
