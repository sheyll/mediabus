-- | Conduit combinators for 'Stream's
module Data.MediaBus.Conduit.Stream
  ( yieldStreamish,
    yieldStreamish',
    yieldNextFrame,
    yieldNextFrame',
    yieldStartFrameCtx,
    yieldStartFrameCtx',
    toFramesC,
    overStreamC,
    mapFramesC,
    mapFramesC',
    mapSeqNumC,
    mapTimestampC,
    mapTimestampC',
    mapFrameContentMC,
    mapFrameContentMC',
    mapFrameContentC,
    mapFrameContentC',
    foldStream,
    foldStreamM,
    concatStreamContents,
    logStreamC,
    logWarnStreamC,
    logInfoStreamC,
    logDebugStreamC,
  )
where

import Conduit
  ( ConduitT,
    MonadTrans (lift),
    Void,
    awaitForever,
    execWriterC,
    mapC,
    mapInput,
    mapMC,
    mapOutput,
    yield,
  )
import Control.Lens (mapMOf, over, (^?))
import Control.Monad ((>=>))
import Control.Monad.Logger
import Control.Monad.Writer.Strict (tell)
import Control.Parallel.Strategies
  ( NFData,
    rdeepseq,
    withStrategy,
  )
import Data.Maybe (fromMaybe)
import Data.MediaBus.Basics.Sequence (HasSeqNum (seqNum))
import Data.MediaBus.Basics.Series (Series (Next, Start), _Next)
import Data.MediaBus.Basics.Ticks (HasTimestamp (timestamp))
import Data.MediaBus.Media.Stream
  ( EachFramePayload (eachFramePayload),
    Frame,
    FrameCtx,
    Stream (..),
    Streamish,
    framePayload,
    stream,
  )
import Data.String
import Data.Text (Text)

-- * Yielding 'Stream' content

-- | Yield a 'Stream' from 'Streamish'
yieldStreamish ::
  Monad m => Streamish i s t p c -> ConduitT a (Stream i s t p c) m ()
yieldStreamish = yield . MkStream

-- | Strict version of 'yieldStreamish'
yieldStreamish' ::
  (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m) =>
  Streamish i s t p c ->
  ConduitT a (Stream i s t p c) m ()
yieldStreamish' = yield . withStrategy rdeepseq . MkStream

-- | Yield the next 'Frame' of a 'Stream'
yieldNextFrame :: Monad m => Frame s t c -> ConduitT a (Stream i s t p c) m ()
yieldNextFrame = yieldStreamish . Next

-- | Strict version of 'yieldNextFrame'
yieldNextFrame' ::
  (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m) =>
  Frame s t c ->
  ConduitT a (Stream i s t p c) m ()
yieldNextFrame' = yieldStreamish' . Next

-- | Yield the starting 'FrameCtx' of a 'Stream'
yieldStartFrameCtx ::
  Monad m => FrameCtx i s t p -> ConduitT a (Stream i s t p c) m ()
yieldStartFrameCtx = yieldStreamish . Start

-- | Strict version of 'yieldStartFrameCtx'
yieldStartFrameCtx' ::
  ( NFData i,
    NFData s,
    NFData t,
    NFData c,
    NFData p,
    NFData (FrameCtx i s t p),
    Monad m
  ) =>
  FrameCtx i s t p ->
  ConduitT a (Stream i s t p c) m ()
yieldStartFrameCtx' = yieldStreamish' . Start

-- * Conversion from/to 'Stream' 'Conduit's

-- | Create a 'Stream' conduit from a 'Streamish' conduit.
overStreamC ::
  Monad m =>
  ConduitT (Streamish i s t p c) (Streamish i' s' t' p' c') m () ->
  ConduitT (Stream i s t p c) (Stream i' s' t' p' c') m ()
overStreamC = mapInput _stream (Just . MkStream) . mapOutput MkStream

-- | A conduit that receives 'Stream's and yields all 'Frames'
toFramesC :: Monad m => ConduitT (Stream i s t p c) (Frame s t c) m ()
toFramesC = awaitForever go
  where
    go (MkStream (Start _)) = return ()
    go (MkStream (Next !frm)) = yield frm

-- * Pure Mapping over 'Streams'

-- | A conduit that applies the given function to every 'Frame' of a 'Stream'.
mapFramesC ::
  Monad m =>
  (Frame s t c -> m (Frame s t c')) ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFramesC !f = mapMC (mapMOf (stream . _Next) f)

-- | Strict version of 'mapFramesC'
mapFramesC' ::
  (NFData i, NFData s, NFData t, NFData c', Monad m) =>
  (Frame s t c -> Frame s t c') ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFramesC' !f = mapC (over (stream . _Next) (withStrategy rdeepseq f))

-- | A conduit that applies the given function to every sequence number of a
-- 'Stream', in 'Frame's as well as 'FrameCtx's.
mapSeqNumC ::
  Monad m =>
  (s -> s') ->
  ConduitT (Stream i s t p c) (Stream i s' t p c) m ()
mapSeqNumC = mapC . over seqNum

-- | A conduit that applies the given function to every time stamp of a
-- 'Stream', in 'Frame's as well as 'FrameCtx's.
mapTimestampC ::
  Monad m =>
  (t -> t') ->
  ConduitT (Stream i s t p c) (Stream i s t' p c) m ()
mapTimestampC = mapC . over timestamp

-- | A strict version of 'mapTimestampC'.
mapTimestampC' ::
  (NFData t, Monad m) =>
  (t -> t') ->
  ConduitT (Stream i s t p c) (Stream i s t' p c) m ()
mapTimestampC' = mapC . withStrategy rdeepseq . over timestamp

-- | A conduit that applies the given pure function to 'eachFramePayload' of a 'Stream'.
mapFrameContentC ::
  (Monad m) =>
  (c -> c') ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFrameContentC !f = mapC (over eachFramePayload f)

-- | A conduit that applies the given pure function to 'eachFramePayload' of a 'Stream' (strict).
mapFrameContentC' ::
  (NFData c', Monad m) =>
  (c -> c') ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFrameContentC' !f = mapC (over eachFramePayload (withStrategy rdeepseq . f))

-- * Monadic mapping over 'Stream' conduits

-- | A conduit that applies the given monadic function to 'eachFramePayload' of a 'Stream'.
mapFrameContentMC ::
  Monad m =>
  (c -> m c') ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFrameContentMC = mapMC . mapMOf eachFramePayload

-- | A strict variant of 'mapFrameContentMC'
mapFrameContentMC' ::
  (NFData (Stream i s t p c'), Monad m) =>
  (c -> m c') ->
  ConduitT (Stream i s t p c) (Stream i s t p c') m ()
mapFrameContentMC' !f =
  mapMC (mapMOf eachFramePayload f >=> return . withStrategy rdeepseq)

-- * Folding over 'Stream' conduits

-- | Like 'Data.Foldable.foldMap' this uses the given function to extract a
-- monoidal value and 'mappend's all results into a single value, which is
-- returned when the 'Conduit' terminates.
foldStream ::
  (Monoid o, Monad m) =>
  (Stream i s t p c -> o) ->
  ConduitT (Stream i s t p c) Void m o
foldStream !f = execWriterC $ awaitForever $ tell . f

-- | Monadic variant of 'foldStream'
foldStreamM ::
  (Monoid o, Monad m) =>
  (Stream i s t p c -> m o) ->
  ConduitT (Stream i s t p c) Void m o
foldStreamM !f = execWriterC $ awaitForever (lift . lift . f >=> tell)

-- | Under the constraint that the stream content is a monoid, fold over the
-- stream appending all frame contents, i.e. 'foldStream' of 'eachFramePayload'.
-- When the conduit finishes the monoidal value is returned.
concatStreamContents ::
  (Monoid c, Monad m) => ConduitT (Stream i s t p c) Void m c
concatStreamContents =
  foldStream (fromMaybe mempty . (^? stream . _Next . framePayload))

-- | Log a stream.
logStreamC ::
  (Show (Stream i s t p c), Monad m, MonadLogger m) =>
  (Stream i s t p c -> Maybe LogLevel) ->
  Text ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
logStreamC f title =
  mapMC
    ( \x -> do
        mapM_ (`logOtherN` (title <> fromString (show x))) (f x)
        return x
    )

-- | Log a stream using 'LevelWarn'.
logWarnStreamC ::
  (Show (Stream i s t p c), Monad m, MonadLogger m) =>
  Text ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
logWarnStreamC =
  logStreamC (const (Just LevelWarn))

-- | Log a stream using 'LevelInfo'.
logInfoStreamC ::
  (Show (Stream i s t p c), Monad m, MonadLogger m) =>
  Text ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
logInfoStreamC =
  logStreamC (const (Just LevelInfo))

-- | Log a stream using 'LevelDebug'.
logDebugStreamC ::
  (Show (Stream i s t p c), Monad m, MonadLogger m) =>
  Text ->
  ConduitT (Stream i s t p c) (Stream i s t p c) m ()
logDebugStreamC =
  logStreamC (const (Just LevelDebug))