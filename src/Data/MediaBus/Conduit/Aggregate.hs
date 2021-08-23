-- | Aggregate payload frames until a certain duration has been reached into a list of
-- payload values.
--
-- This is for payload that cannot be easily cut into smaller segments like
-- raw pcm audio, for that, use "Data.MediaBus.Conduit.Segment".
module Data.MediaBus.Conduit.Aggregate
  ( aggregateDurationC,
    aggregateCountC,
  )
where

import Conduit (ConduitT, await)
import Control.Lens
import Data.MediaBus.Basics.Monotone (LocalOrd)
import Data.MediaBus.Basics.Series (Series (Next, Start))
import Data.MediaBus.Basics.Ticks
  ( CanBeTicks,
    HasDuration (..),
    HasTimestamp (timestamp'),
    Ticks,
  )
import Data.MediaBus.Conduit.Stream
  ( yieldNextFrame, yieldStartFrameCtx
  )
import Data.MediaBus.Media.Stream
  ( Frame (MkFrame),
    Stream (MkStream),
    framePayload,
    frameSeqNum,
  )
import Numeric.Natural (Natural)

-- | Group content frames into a list of frames such that the total duration
-- of the contents is just @>= t@.
--
-- If you want a specific number of contents, and the contents have no 'HasDuration'
-- instance, use 'aggregateCountC'.
--
-- The sequence numbers are taken from the the first content of each aggregate,
-- and thus may not be monotonic incrementing by one.
--
-- The timestamps are taken from the the first content of each aggregate.
--
-- When a start frame is received, the current aggregate is sent,
-- even if it is incomplete, and the start frame is passed through.
aggregateDurationC ::
  forall f i s r t p c m.
  ( Monad m,
    HasDuration c,
    Applicative f,
    Semigroup (f c),
    LocalOrd t,
    CanBeTicks r t
  ) =>
  -- | The minimum total duration of the list of payloads.
  Ticks r t ->
  ConduitT
    (Stream i s (Ticks r t) p c)
    (Stream i s (Ticks r t) p (f c))
    m
    ()
aggregateDurationC t = go Nothing 0
  where
    go mAcc0 accDur0 =
      await >>= maybe (yieldAccum mAcc0) handlePayload
      where
        handlePayload = \case
          MkStream (Start pl) -> handleStart pl
          MkStream (Next pl) -> handleNext pl

        handleStart f = do
          yieldAccum mAcc0
          yieldStartFrameCtx f
          go Nothing 0

        handleNext (MkFrame !fSeq !fTs !fContent) =
          let !accDur = accDur0 + getDurationTicks fContent
              !acc =
                maybe
                  (Just (fSeq, fTs, pure fContent))
                  (Just . over _3 (<> pure fContent))
                  mAcc0
           in if accDur < t
                then go acc accDur
                else do
                  yieldAccum acc
                  go Nothing 0

    yieldAccum = \case
      Nothing -> return ()
      Just (!s, !ts, !c) -> yieldNextFrame (MkFrame s ts c)

-- | Group a specific count of content frames into a list of frames.
--
-- If you want a specific duration of contents, and the contents have a 'HasDuration'
-- instance, use 'aggregateDurationC'.
--
-- The sequence numbers are taken from the the first content of each aggregate,
-- and thus may not be monotonic incrementing by one.
--
-- The timestamps are taken from the the first content of each aggregate.
--
-- When a start frame is received, the current aggregate is sent,
-- even if it is incomplete, and the start frame is passed through.
aggregateCountC ::
  forall f i s t p c m.
  ( Monad m,
    Semigroup (f c),
    Applicative f
  ) =>
  -- | The minimum total duration of the list of payloads.
  Natural ->
  ConduitT
    (Stream i s t p c)
    (Stream i s t p (f c))
    m
    ()
aggregateCountC maxCount = go Nothing 0
  where
    go mAcc0 counter0 =
      await >>= maybe (yieldAccum mAcc0) handlePayload
      where
        handlePayload = \case
          MkStream (Start pl) -> handleStart pl
          MkStream (Next pl) -> handleNext pl

        handleStart pl = do
          yieldAccum mAcc0
          yieldStartFrameCtx pl
          go Nothing 0

        handleNext f =
          let !counter = counter0 + 1
              !acc =
                maybe
                  (Just (view timestamp' f, view frameSeqNum f, pure (view framePayload f)))
                  (Just . over _3 (<> pure (view framePayload f)))
                  mAcc0
           in if counter < maxCount
                then go acc counter
                else do
                  yieldAccum acc
                  go Nothing 0

    yieldAccum = \case
      Nothing -> return ()
      Just (s, ts, c) -> yieldNextFrame (MkFrame s ts c)
