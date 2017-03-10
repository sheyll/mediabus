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
  , pushedDuration
  , generateFrame
  , type ReframerT
  , ReframerSt()
  , ReframeError(..)
  , mkReFrameError
  ) where

import Control.Exception
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

-- | Reset the current timing and sequence number, and start as if the given
-- frame was the first frame.
pushStartFrame
  :: (Num d, Monad m)
  => Frame s d d -> ReframerT m s d ()
pushStartFrame (MkFrame t s d) = State.put (MkTimeFrame t s (t + d))

-- | Increase the available duration by the duration in the frame,  iff the
-- timestamp of the given frame matches exactly the timestamp after the end
-- of the available period, otherwise do nothing with the state and return
-- 'True'.
pushFrame
  :: (Num d, Monad m, Eq d)
  => Frame s d d -> ReframerT m s d Bool
pushFrame (MkFrame ts _ dur) = do
  (MkTimeFrame startTs nextSeq endTs) <- State.get
  let endTs' = ts + dur
  if ts /= endTs
    then return False
    else do
      State.put (MkTimeFrame startTs nextSeq endTs')
      return True

-- | Return the duration of the frames recorded with 'pushFrame'.
pushedDuration
  :: (Num d, Monad m)
  => ReframerT m s d d
pushedDuration = do
  (MkTimeFrame startTs _ endTs) <- State.get
  return (endTs - startTs)

-- | Try to create a frame with the given duration, and update the state
-- accordingly, if the given length is not available return 'Nothing'
generateFrame
  :: (Num s, Num d, Monad m, Eq d, Ord d, Show d)
  => d -> ReframerT m s d (Maybe (Frame s d ()))
generateFrame wantedDuration = do
  (MkTimeFrame startTs nextSeq endTs) <- State.get
  available <- pushedDuration
  if wantedDuration >= available
    then do
      State.put (MkTimeFrame (startTs + wantedDuration) (nextSeq + 1) endTs)
      return (Just (MkFrame startTs nextSeq ()))
    else return Nothing

-- | Reframer state.
data ReframerSt s d = MkTimeFrame
  { _startTs :: !d
  , _nextSeqNum :: !s
  , _endTs :: !d
  } deriving (Show, Typeable)

-- | The 'ReframerSt' 'StateT' transformer
type ReframerT m s d a = StateT (ReframerSt s d) m a

-- | The exception type for 'encodeLinearToAacC'
data ReframeError s d = MkReframeError
  { reframeError :: String
  , reframeErrorRequestedOutput :: Maybe d
  , reframeErrorSt :: ReframerSt s d
  } deriving (Show, Typeable)

instance (Show s, Typeable s, Show d, Typeable d) =>
         Exception (ReframeError s d)

-- | Utility function to generate a 'ReframeError' with the current state.
mkReFrameError
  :: Monad m
  => String -> Maybe d -> ReframerT m s d (ReframeError s d)
mkReFrameError msg requestedOutputDuration =
  MkReframeError msg requestedOutputDuration <$> State.get
