module Data.MediaBus.Stream
    ( HasPayload(..)
    , FrameCtx(..)
    , frameCtxSourceId
    , frameCtxSeqNumRef
    , frameCtxTimestampRef
    , frameCtxInit
    , Frame(..)
    , frameSeqNum
    , frameTimestamp
    , framePayload
    , Stream(..)
    , stream
    , yieldStreamish
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
    , mapPayloadMC
    , mapPayloadMC'
    , mapPayloadC'
    , convertTicksC'
    , foldStream
    , foldStreamM
    , concatStreamContents
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.Sequence
import           Data.MediaBus.Payload
import           Data.MediaBus.Ticks
import           Data.MediaBus.Series
import           Control.Monad.Writer.Strict ( tell )
import           Data.Maybe
import           Test.QuickCheck
import           Data.Default
import           Text.Printf
import           GHC.TypeLits
import           GHC.Generics                ( Generic )
import           Control.Parallel.Strategies ( NFData, rdeepseq, withStrategy )

-- TODO remove Ticks' or create Ticks32/64/...
data FrameCtx i s t p = MkFrameCtx { _frameCtxSourceId     :: !i
                                   , _frameCtxTimestampRef :: !t
                                   , _frameCtxSeqNumRef    :: !s
                                   , _frameCtxInit         :: !p
                                   }
    deriving (Eq, Ord, Generic)

instance (NFData i, NFData s, NFData t, NFData p) =>
         NFData (FrameCtx i s t p)

makeLenses ''FrameCtx

instance HasTimestampT (FrameCtx i s t p) where
    type GetTimestamp (FrameCtx i s t p) = t
    type SetTimestamp (FrameCtx i s t p) t' = (FrameCtx i s t' p)

instance HasTimestamp (FrameCtx i s t p) where
    timestamp = frameCtxTimestampRef

instance HasSeqNumT (FrameCtx i s t p) where
    type GetSeqNum (FrameCtx i s t p) = s
    type SetSeqNum (FrameCtx i s t p) x = FrameCtx i x t p

instance HasDuration (FrameCtx i s t p) where
    getDuration _ = 0

instance HasSeqNum (FrameCtx i s t p) where
    seqNum = frameCtxSeqNumRef

instance (Arbitrary i, Arbitrary s, Arbitrary t, Arbitrary p) =>
         Arbitrary (FrameCtx i s t p) where
    arbitrary = MkFrameCtx <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance (Default i, Default s, Default t, Default p) =>
         Default (FrameCtx i s t p) where
    def = MkFrameCtx def def def def

instance (Show i, Show s, Show t, Show p) =>
         Show (FrameCtx i s t p) where
    show (MkFrameCtx sid tsr snr spr) =
        printf "FRAME-CTX: %15s | %15s | %15s | %s"
               (show sid)
               (show snr)
               (show tsr)
               (show spr)

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Frame s t c = MkFrame { _frameTimestamp :: !t
                           , _frameSeqNum    :: !s
                           , _framePayload   :: !c
                           }
    deriving (Eq, Ord, Generic)

instance (NFData c, NFData s, NFData t) =>
         NFData (Frame s t c)

deriving instance Functor (Frame s t)

makeLenses ''Frame

instance HasPayload (Frame s t c) where
    type GetPayload (Frame s t c) = c
    type SetPayload (Frame s t c) d = Frame s t d
    payload = framePayload

instance HasTimestampT (Frame s t c) where
    type GetTimestamp (Frame s t c) = t
    type SetTimestamp (Frame s t c) t' = Frame s t' c

instance HasTimestamp (Frame s t c) where
    timestamp = frameTimestamp

instance HasSeqNumT (Frame s t c) where
    type GetSeqNum (Frame s t c) = s
    type SetSeqNum (Frame s t c) x = Frame x t c

instance HasSeqNum (Frame s t c) where
    seqNum = frameSeqNum

instance HasDuration c =>
         HasDuration (Frame s t c) where
    getDuration = getDuration . _framePayload

instance (Arbitrary c, Arbitrary s, Arbitrary t) =>
         Arbitrary (Frame s t c) where
    arbitrary = MkFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance (Default s, Default t, Default c) =>
         Default (Frame s t c) where
    def = MkFrame def def def

instance (Show s, Show t, Show v) =>
         Show (Frame s t v) where
    show (MkFrame ts sn v) =
        printf "FRAME: %15s | %15s | %s" (show sn) (show ts) (show v)

newtype Stream i s t p c = MkStream { _stream :: Streamish i s t p c }
    deriving (Ord, Eq, Arbitrary, Generic)

instance (NFData i, NFData s, NFData t, NFData c, NFData p) =>
         NFData (Stream i s t p c)

type Streamish i s t p c = Series (FrameCtx i s t p) (Frame s t c)

makeLenses ''Stream

instance HasPayload (Stream i s t p c) where
    type GetPayload (Stream i s t p c) = c
    type SetPayload (Stream i s t p c) d = Stream i s t p d
    payload = stream . _Next . payload

instance HasDuration c =>
         HasDuration (Stream i s t p c) where
    getDuration = maybe 0 getDuration . preview (stream . _Next)

instance HasSeqNumT (Stream i s t p c) where
    type GetSeqNum (Stream i s t p c) = s
    type SetSeqNum (Stream i s t p c) x = Stream i x t p c

instance HasSeqNum (Stream i s t p c) where
    seqNum = stream . seqNum

instance HasTimestampT (Stream i s t p c) where
    type GetTimestamp (Stream i s t p c) = t
    type SetTimestamp (Stream i s t p c) t' = Stream i s t' p c

instance HasTimestamp (Stream i s t p c) where
    timestamp = stream . timestamp

instance (Default c, Default s, Default t) =>
         Default (Stream i s t p c) where
    def = MkStream (Next (MkFrame def def def))

instance (Show i, Show s, Show t, Show c, Show p) =>
         Show (Stream i s t p c) where
    show (MkStream s) = show s

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

mapPayloadMC :: Monad m
             => (c -> m c')
             -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapPayloadMC = mapMC . mapMOf payload

mapPayloadMC' :: (NFData (Stream i s t p c'), Monad m)
              => (c -> m c')
              -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapPayloadMC' !f = mapMC (mapMOf payload f >=> return . withStrategy rdeepseq)

mapPayloadC' :: (NFData c', Monad m)
             => (c -> c')
             -> Conduit (Stream i s t p c) m (Stream i s t p c')
mapPayloadC' !f = mapC (over payload (withStrategy rdeepseq . f))

convertTicksC' :: forall proxy0 proxy1 m r t r' t' i s c p.
               (NFData t, NFData t', KnownNat r, KnownNat r', Integral t, Integral t', Monad m, NFData t')
               => proxy0 '(r, t)
               -> proxy1 '(r', t')
               -> Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r' t') p c)
convertTicksC' _ _ = mapTicksC' convertTicks

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
