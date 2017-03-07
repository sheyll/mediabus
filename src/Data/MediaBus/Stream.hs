-- | Conduit combinators for 'Stream's
module Data.MediaBus.Stream
  ( yieldStreamish
  , yieldStreamish'
  , yieldNextFrame
  , yieldNextFrame'
  , yieldStartFrameCtx
  , yieldStartFrameCtx'
  , toFramesC
  , overStreamC
  , mapFramesC
  , mapFramesC'
  , mapSeqNumC
  , mapTicksC
  , mapTicksC'
  , mapFrameContentMC
  , mapFrameContentMC'
  , mapFrameContentC'
  , foldStream
  , foldStreamM
  , concatStreamContents
  ) where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Writer.Strict (tell)
import Control.Parallel.Strategies (NFData, rdeepseq, withStrategy)
import Data.Maybe
import Data.MediaBus.Basics.Sequence
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Stream

-- * Yielding 'Stream' content
-- | Yield a 'Stream' from 'Streamish'
yieldStreamish
  :: Monad m
  => Streamish i s t p c -> Conduit a m (Stream i s t p c)
yieldStreamish = yield . MkStream

-- | Strict version of 'yieldStreamish'
yieldStreamish'
  :: (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m)
  => Streamish i s t p c -> Conduit a m (Stream i s t p c)
yieldStreamish' = yield . withStrategy rdeepseq . MkStream

-- | Yield the next 'Frame' of a 'Stream'
yieldNextFrame
  :: Monad m
  => Frame s t c -> Conduit a m (Stream i s t p c)
yieldNextFrame = yieldStreamish . Next

-- | Strict version of 'yieldNextFrame'
yieldNextFrame'
  :: (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m)
  => Frame s t c -> Conduit a m (Stream i s t p c)
yieldNextFrame' = yieldStreamish' . Next

-- | Yield the starting 'FrameCtx' of a 'Stream'
yieldStartFrameCtx
  :: Monad m
  => FrameCtx i s t p -> Conduit a m (Stream i s t p c)
yieldStartFrameCtx = yieldStreamish . Start

-- | Strict version of 'yieldStartFrameCtx'
yieldStartFrameCtx'
  :: ( NFData i
     , NFData s
     , NFData t
     , NFData c
     , NFData p
     , NFData (FrameCtx i s t p)
     , Monad m
     )
  => FrameCtx i s t p -> Conduit a m (Stream i s t p c)
yieldStartFrameCtx' = yieldStreamish' . Start

-- * Conversion from/to 'Stream' 'Conduit's
-- | Create a 'Stream' conduit from a 'Streamish' conduit.
overStreamC
  :: Monad m
  => Conduit (Streamish i s t p c) m (Streamish i' s' t' p' c')
  -> Conduit (Stream i s t p c) m (Stream i' s' t' p' c')
overStreamC = mapInput _stream (Just . MkStream) . mapOutput MkStream

-- | A conduit that receives 'Stream's and yields all 'Frames'
toFramesC
  :: Monad m
  => Conduit (Stream i s t p c) m (Frame s t c)
toFramesC = awaitForever go
  where
    go (MkStream (Start _)) = return ()
    go (MkStream (Next !frm)) = yield frm

-- * Pure Mapping over 'Streams'
-- | A conduit that applies the given function to every 'Frame' of a 'Stream'.
mapFramesC
  :: Monad m
  => (Frame s t c -> m (Frame s t c'))
  -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFramesC !f = mapMC (mapMOf (stream . _Next) f)

-- | Strict version of 'mapFramesC'
mapFramesC'
  :: (NFData i, NFData s, NFData t, NFData c', Monad m)
  => (Frame s t c -> Frame s t c')
  -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFramesC' !f = mapC (over (stream . _Next) (withStrategy rdeepseq f))

-- | A conduit that applies the given function to every sequence number of a
-- 'Stream', in 'Frame's as well as 'FrameCtx's.
mapSeqNumC
  :: Monad m
  => (s -> s') -> Conduit (Stream i s t p c) m (Stream i s' t p c)
mapSeqNumC = mapC . over seqNum

-- | A conduit that applies the given function to every time stamp of a
-- 'Stream', in 'Frame's as well as 'FrameCtx's.
mapTicksC
  :: Monad m
  => (t -> t') -> Conduit (Stream i s t p c) m (Stream i s t' p c)
mapTicksC = mapC . over timestamp

-- | A strict version of 'mapTicksC'.
mapTicksC'
  :: (NFData t, Monad m)
  => (t -> t') -> Conduit (Stream i s t p c) m (Stream i s t' p c)
mapTicksC' = mapC . withStrategy rdeepseq . over timestamp

-- | A conduit that applies the given pure function to 'eachFrameContent' of a 'Stream'.
mapFrameContentC'
  :: (NFData c', Monad m)
  => (c -> c') -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentC' !f = mapC (over eachFrameContent (withStrategy rdeepseq . f))

-- * Monadic mapping over 'Stream' conduits
-- | A conduit that applies the given monadic function to 'eachFrameContent' of a 'Stream'.
mapFrameContentMC
  :: Monad m
  => (c -> m c') -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentMC = mapMC . mapMOf eachFrameContent

-- | A strict variant of 'mapFrameContentMC'
mapFrameContentMC'
  :: (NFData (Stream i s t p c'), Monad m)
  => (c -> m c') -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentMC' !f =
  mapMC (mapMOf eachFrameContent f >=> return . withStrategy rdeepseq)

-- * Folding over 'Stream' conduits
-- | Like 'Data.Foldable.foldMap' this uses the given function to extract a
-- monoidal value and 'mappend's all results into a single value, which is
-- returned when the 'Conduit' terminates.
foldStream
  :: (Monoid o, Monad m)
  => (Stream i s t p c -> o) -> Sink (Stream i s t p c) m o
foldStream !f = execWriterC $ awaitForever $ tell . f

-- | Monadic variant of 'foldStream'
foldStreamM
  :: (Monoid o, Monad m)
  => (Stream i s t p c -> m o) -> Sink (Stream i s t p c) m o
foldStreamM !f = execWriterC $ awaitForever (lift . lift . f >=> tell)

-- | Under the constraint that the stream content is a monoid, fold over the
-- stream appending all frame contents, i.e. 'foldStream' of 'eachFrameContent'.
-- When the conduit finishes the monoidal value is returned.
concatStreamContents
  :: (Monoid c, Monad m)
  => Sink (Stream i s t p c) m c
concatStreamContents =
  foldStream (fromMaybe mempty . (^? stream . _Next . framePayload))
