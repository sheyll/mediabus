-- | Conduits for 'Stream's
module Data.MediaBus.Stream
    (

      yieldStreamish
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
    , convertTicksC'
    , deriveFrameTimestamp
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.Basics.Sequence
import           Data.MediaBus.Media.Stream
import           Data.MediaBus.Basics.Ticks
import           Data.MediaBus.Basics.Series
import           Control.Monad.Writer.Strict ( tell )
import           Control.Monad.State.Strict
import           Data.Maybe
import           Control.Parallel.Strategies ( NFData, rdeepseq, withStrategy )

yieldStreamish :: Monad m
               => Streamish i s t p c
               -> Conduit a m (Stream i s t p c)
yieldStreamish = yield . MkStream

yieldStreamish' :: (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m)
                => Streamish i s t p c
                -> Conduit a m (Stream i s t p c)
yieldStreamish' = yield . withStrategy rdeepseq . MkStream

yieldNextFrame :: Monad m => Frame s t c -> Conduit a m (Stream i s t p c)
yieldNextFrame = yieldStreamish . Next

yieldNextFrame' :: (NFData i, NFData s, NFData t, NFData c, NFData p, Monad m)
                => Frame s t c
                -> Conduit a m (Stream i s t p c)
yieldNextFrame' = yieldStreamish' . Next

yieldStartFrameCtx :: Monad m
                   => FrameCtx i s t p
                   -> Conduit a m (Stream i s t p c)
yieldStartFrameCtx = yieldStreamish . Start

yieldStartFrameCtx' :: (NFData i, NFData s, NFData t, NFData c, NFData p, NFData (FrameCtx i s t p), Monad m)
                    => FrameCtx i s t p
                    -> Conduit a m (Stream i s t p c)
yieldStartFrameCtx' = yieldStreamish' . Start

overStreamC :: Monad m
            => Conduit (Series (FrameCtx i s t p) (Frame s t c)) m (Series (FrameCtx i' s' t' p') (Frame s' t' c'))
            -> Conduit (Stream i s t p c) m (Stream i' s' t' p' c')
overStreamC = mapInput _stream (Just . MkStream) . mapOutput MkStream

toFramesC :: Monad m => Conduit (Stream i s t p c) m (Frame s t c)
toFramesC = awaitForever go
  where
    go (MkStream (Start _)) =
        return ()
    go (MkStream (Next !frm)) =
        yield frm

mapFramesC' :: (NFData i, NFData s, NFData t, NFData c', Monad m)
            => (Frame s t c -> Frame s t c')
            -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFramesC' !f = mapC (over (stream . _Next) (withStrategy rdeepseq f))

mapFramesC :: Monad m
           => (Frame s t c -> m (Frame s t c'))
           -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFramesC !f = mapMC (mapMOf (stream . _Next) f)

mapSeqNumC :: Monad m
           => (s -> s')
           -> Conduit (Stream i s t p c) m (Stream i s' t p c)
mapSeqNumC = mapC . over seqNum

mapTicksC :: Monad m
          => (t -> t')
          -> Conduit (Stream i s t p c) m (Stream i s t' p c)
mapTicksC = mapC . over timestamp

mapTicksC' :: (NFData t, Monad m)
           => (t -> t')
           -> Conduit (Stream i s t p c) m (Stream i s t' p c)
mapTicksC' = mapC . withStrategy rdeepseq . over timestamp

mapFrameContentMC :: Monad m
             => (c -> m c')
             -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentMC = mapMC . mapMOf eachFrameContent

mapFrameContentMC' :: (NFData (Stream i s t p c'), Monad m)
              => (c -> m c')
              -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentMC' !f = mapMC (mapMOf eachFrameContent f >=> return . withStrategy rdeepseq)

mapFrameContentC' :: (NFData c', Monad m)
             => (c -> c')
             -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapFrameContentC' !f = mapC (over eachFrameContent (withStrategy rdeepseq . f))

foldStream :: (Monoid o, Monad m)
           => (Stream i s t p c -> o)
           -> Sink (Stream i s t p c) m o
foldStream !f = execWriterC $
    awaitForever $
        tell .
            f

foldStreamM :: (Monoid o, Monad m)
            => (Stream i s t p c -> m o)
            -> Sink (Stream i s t p c) m o
foldStreamM !f = execWriterC $
    awaitForever (lift . lift . f >=> tell)

concatStreamContents :: (Monoid c, Monad m) => Sink (Stream i s t p c) m c
concatStreamContents = foldStream (fromMaybe mempty .
                                       (^? stream .
                                               _Next .
                                                   framePayload))


-- * Media Data Synchronization TODO move back to Ticks??

-- | Overwrite the timestamp of a stream of things that  have a time stamp field
--  (i.e. 'HasTimestamp' instances)  and also a duration, such that the
--  timestamps increment by the duration starting from 0.
deriveFrameTimestamp :: forall m r t a. (Monad m, CanBeTicks r t, HasDuration a, HasTimestamp a)
                     => Ticks r t
                     -> Conduit a m (SetTimestamp a (Ticks r t))
deriveFrameTimestamp t0 =
    evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync :: a -> Conduit a (StateT (Ticks r t) m) (SetTimestamp a (Ticks r t))
    yieldSync sb = do
        t <- get
        modify (+ (nominalDiffTime # getDuration sb))
        yield (sb & timestamp .~ t)

-- | Recalculate all timestamps in a 'Stream'
convertTicksC' :: forall proxy0 proxy1 m r t r' t' i s c p.
               (NFData t, NFData t', CanBeTicks r t, CanBeTicks r' t', Monad m, NFData t')
               => proxy0 '(r, t)
               -> proxy1 '(r', t')
               -> Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r' t') p c)
convertTicksC' _ _ = mapTicksC' convertTicks
