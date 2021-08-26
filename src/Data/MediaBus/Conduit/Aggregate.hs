-- | Aggregate payload frames until a certain duration has been reached into a list of
-- payload values.
--
-- This is for payload that cannot be easily cut into smaller segments like
-- raw pcm audio, for that, use "Data.MediaBus.Conduit.Segment".
module Data.MediaBus.Conduit.Aggregate
  ( aggregateDurationC,
    aggregateCountC,
    Frames (..),
    frames,
    aggregateByC,
    AggregationResult (..),
  )
where

import Conduit (ConduitT, await)
import Control.DeepSeq
import Control.Lens
import Data.Foldable (traverse_)
import Data.MediaBus.Basics.Series (Series (Next, Start))
import Data.MediaBus.Basics.Ticks
  ( HasDuration (..),
  )
import Data.MediaBus.Conduit.Stream
  ( yieldNextFrame,
    yieldStartFrameCtx,
  )
import Data.MediaBus.Media.Channels
import Data.MediaBus.Media.Media
import Data.MediaBus.Media.Stream
import Data.Time (NominalDiffTime)
import Numeric.Natural (Natural)

-- | A list of 'Frame' values aggregated by 'aggregateCountC'
-- or 'aggregateDurationC'.
newtype Frames sequenceNumber timestamp payload = MkFrames {_frames :: [Frame sequenceNumber timestamp payload]}
  deriving (Eq, Ord, NFData, Functor, Monoid, Semigroup)

makeLenses ''Frames

instance
  (Show s, Show t, HasDuration p) =>
  Show (Frames s t p)
  where
  showsPrec d f@(MkFrames fs) =
    case fs of
      [] -> showString "no aggregated frames"
      (f1 : _) ->
        showParen
          (d >= 10)
          ( showString "frames: starting with "
              . shows (f1 ^. frameSeqNum)
              . showString " "
              . shows (f1 ^. frameTimestamp)
              . showString " aggregated frames: "
              . shows (length fs)
              . showString " total duration: "
              . shows (getDuration f)
          )

instance EachFramePayload (Frames s t c) (Frames s t c') where
  type FramePayloadFrom (Frames s t c) = c
  type FramePayloadTo (Frames s t c') = c'
  eachFramePayload = frames . each . eachFramePayload

instance Foldable (Frames sequenceNumber timestamp) where
  foldr = foldrOf eachFramePayload

instance Traversable (Frames sequenceNumber timestamp) where
  traverse = eachFramePayload

instance
  (EachChannel c c') =>
  EachChannel (Frames s t c) (Frames s t c')
  where
  type ChannelsFrom (Frames s t c) = ChannelsFrom c
  type ChannelsTo (Frames s t c') = ChannelsTo c'
  eachChannel = eachFramePayload . eachChannel

instance
  EachMedia c c' =>
  EachMedia (Frames s t c) (Frames s t c')
  where
  type MediaFromE (Frames s t c) = MediaFromE c
  type MediaToE (Frames s t c') = MediaToE c'
  eachMedia = eachFramePayload . eachMedia

instance
  HasDuration c =>
  HasDuration (Frames s t c)
  where
  getDuration = sumOf (eachFramePayload . to getDuration)

-- | The value used to aggregate 'Frames' using 'aggregateByC'.
data AggregationResult s t c a = MkAggregationResult
  { aggregationState :: !a,
    aggregationUnfinished :: !(Frames s t c)
  }

-- | Group content frames into 'Frames' using a grouping function.
aggregateByC ::
  forall a i s t p c m.
  ( Monad m
  ) =>
  -- | Process 'Just' a 'Frame', and the current @AggregationResult@
  -- return 'Maybe' 'Frames' to yield and an 'AggregationResult'.
  -- When a start frame is received, or when the conduit ends, the first
  -- parameter is 'Nothing'.
  (Maybe (Frame s t c) -> AggregationResult s t c a -> (Maybe (Frames s t c), AggregationResult s t c a)) ->
  a ->
  ConduitT
    (Stream i s t p c)
    (Stream i () () p (Frames s t c))
    m
    ()
aggregateByC f = go . arInitial
  where
    arInitial !initialA =
      MkAggregationResult
        { aggregationState = initialA,
          aggregationUnfinished = mempty
        }

    go !ar = await >>= maybe handleEOF handlePayload
      where
        handleEOF =
          let (!mOut, _) = f Nothing ar
           in mapM_ yieldResult mOut

        handlePayload = \case
          MkStream (Start !pl) -> handleStart pl
          MkStream (Next !pl) -> handleNext pl

        handleStart !pl = do
          let (!mRes, !ar') = f Nothing ar
          traverse_ yieldResult mRes
          yieldStartFrameCtx
            ( pl & frameCtxSeqNumRef .~ ()
                & frameCtxTimestampRef .~ ()
            )
          go ar'

        handleNext !pl = do
          let (!mRes, !ar') = f (Just pl) ar
          traverse_ yieldResult mRes
          go ar'

    yieldResult !x = yieldNextFrame (MkFrame () () x)

-- | Group content frames into 'Frames' such that the total duration
-- of the contents is just @>= t@.
--
-- If you want a specific number of contents, and the contents have no 'HasDuration'
-- instance, use 'aggregateCountC'.
--
-- When a start frame is received, the current aggregate is sent,
-- even if it is incomplete, and the start frame is passed through.
aggregateDurationC ::
  forall i s t p c m.
  ( Monad m,
    HasDuration c
  ) =>
  -- | The minimum total duration of the list of payloads.
  NominalDiffTime ->
  ConduitT
    (Stream i s t p c)
    (Stream i () () p (Frames s t c))
    m
    ()
aggregateDurationC t = aggregateByC f (0 :: NominalDiffTime)
  where
    f Nothing ar =
      ( if null (aggregationUnfinished ar)
          then Nothing
          else Just (aggregationUnfinished ar),
        ar
          { aggregationUnfinished = mempty,
            aggregationState = 0
          }
      )
    f (Just pl) ar =
      let !accDur = aggregationState ar + getDuration pl
          !single = MkFrames [pl]
          !acc = aggregationUnfinished ar <> single
       in if accDur < t
            then (Nothing, MkAggregationResult {aggregationState = accDur, aggregationUnfinished = acc})
            else (Just acc, MkAggregationResult {aggregationState = 0, aggregationUnfinished = mempty})

-- | Group a specific number of content frames into a list of frames.
--
-- If you want to aggregate a specific duration of contents, and the
-- contents have a 'HasDuration' instance, use 'aggregateDurationC'.
--
-- When a start frame is received, the current aggregate is sent,
-- even if it is incomplete, and the start frame is passed through.
aggregateCountC ::
  forall i s t p c m.
  ( Monad m
  ) =>
  -- | The minimum total duration of the list of payloads.
  Natural ->
  ConduitT
    (Stream i s t p c)
    (Stream i () () p (Frames s t c))
    m
    ()
aggregateCountC maxCount = aggregateByC f 0
  where
    f Nothing ar =
      ( if null (aggregationUnfinished ar)
          then Nothing
          else Just (aggregationUnfinished ar),
        ar
          { aggregationUnfinished = mempty,
            aggregationState = 0
          }
      )
    f (Just pl) ar =
      let !count = aggregationState ar + 1
          !single = MkFrames [pl]
          !acc = aggregationUnfinished ar <> single
       in if count < maxCount
            then (Nothing, MkAggregationResult {aggregationState = count, aggregationUnfinished = acc})
            else (Just acc, MkAggregationResult {aggregationState = 0, aggregationUnfinished = mempty})
