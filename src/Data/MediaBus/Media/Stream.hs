module Data.MediaBus.Media.Stream
  ( FrameCtx(..)
  , frameCtxSourceId
  , frameCtxSeqNumRef
  , frameCtxTimestampRef
  , frameCtxInit
  , Frame(..)
  , EachFrameContent(..)
  , frameSeqNum
  , frameTimestamp
  , framePayload
  , Stream(..)
  , type Streamish
  , stream
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Default
import Data.MediaBus.Basics.Sequence
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media
import Data.MediaBus.Media.Channels
import GHC.Generics (Generic)
import Test.QuickCheck
import Text.Printf

data FrameCtx i s t p = MkFrameCtx
  { _frameCtxSourceId :: !i
  , _frameCtxTimestampRef :: !t
  , _frameCtxSeqNumRef :: !s
  , _frameCtxInit :: !p
  } deriving (Eq, Ord, Generic)

instance (NFData i, NFData s, NFData t, NFData p) =>
         NFData (FrameCtx i s t p)

makeLenses ''FrameCtx

instance HasTimestamp (FrameCtx i s t p) where
  type GetTimestamp (FrameCtx i s t p) = t
  type SetTimestamp (FrameCtx i s t p) t' = (FrameCtx i s t' p)
  timestamp = frameCtxTimestampRef

instance HasDuration (FrameCtx i s t p) where
  getDuration _ = 0

instance HasSeqNum (FrameCtx i s t p) where
  type GetSeqNum (FrameCtx i s t p) = s
  type SetSeqNum (FrameCtx i s t p) x = FrameCtx i x t p
  seqNum = frameCtxSeqNumRef

instance (Arbitrary i, Arbitrary s, Arbitrary t, Arbitrary p) =>
         Arbitrary (FrameCtx i s t p) where
  arbitrary = MkFrameCtx <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Default i, Default s, Default t, Default p) =>
         Default (FrameCtx i s t p) where
  def = MkFrameCtx def def def def

instance (Show i, Show s, Show t, Show p) =>
         Show (FrameCtx i s t p) where
  show (MkFrameCtx sid tsr snr spr) =
    printf
      "FRAME-CTX: %15s | %15s | %15s | %s"
      (show sid)
      (show snr)
      (show tsr)
      (show spr)

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Frame s t c = MkFrame
  { _frameTimestamp :: !t
  , _frameSeqNum :: !s
  , _framePayload :: !c
  } deriving (Eq, Ord, Generic)

instance (NFData c, NFData s, NFData t) =>
         NFData (Frame s t c)

deriving instance Functor (Frame s t)

makeLenses ''Frame

-- | Class for types that have a 'Traversal' for '_framePayload'
class EachFrameContent s t where
  type FrameContentFrom s
  type FrameContentTo t
  eachFrameContent :: Traversal s t (FrameContentFrom s) (FrameContentTo t)

instance EachFrameContent (Frame s t c) (Frame s t c') where
  type FrameContentFrom (Frame s t c) = c
  type FrameContentTo (Frame s t c') = c'
  eachFrameContent = framePayload

instance (EachChannel c c') =>
         EachChannel (Frame s t c) (Frame s t c') where
  type ChannelsFrom (Frame s t c) = c
  type ChannelsTo (Frame s t c') = c'
  eachChannel = framePayload

instance (HasMedia c c') =>
         HasMedia (Frame s t c) (Frame s t c') where
  type MediaFrom (Frame s t c) = MediaFrom c
  type MediaTo (Frame s t c') = MediaTo c'
  media = framePayload . media

instance HasTimestamp (Frame s t c) where
  type GetTimestamp (Frame s t c) = t
  type SetTimestamp (Frame s t c) t' = Frame s t' c
  timestamp = frameTimestamp

instance HasSeqNum (Frame s t c) where
  type GetSeqNum (Frame s t c) = s
  type SetSeqNum (Frame s t c) x = Frame x t c
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

newtype Stream i s t p c = MkStream
  { _stream :: Streamish i s t p c
  } deriving (Ord, Eq, Arbitrary, Generic)

instance (NFData i, NFData s, NFData t, NFData c, NFData p) =>
         NFData (Stream i s t p c)

type Streamish i s t p c = Series (FrameCtx i s t p) (Frame s t c)

makeLenses ''Stream

instance EachChannel (Frame s t c) (Frame s t c') =>
         EachChannel (Stream i s t p c) (Stream i s t p c') where
  type ChannelsFrom (Stream i s t p c) = ChannelsFrom (Frame s t c)
  type ChannelsTo (Stream i s t p c') = ChannelsTo (Frame s t c')
  eachChannel = stream . _Next . eachChannel

instance HasDuration c =>
         HasDuration (Stream i s t p c) where
  getDuration = maybe 0 getDuration . preview (stream . _Next)

instance HasSeqNum (Stream i s t p c) where
  type GetSeqNum (Stream i s t p c) = s
  type SetSeqNum (Stream i s t p c) x = Stream i x t p c
  seqNum = stream . seqNum

instance HasTimestamp (Stream i s t p c) where
  type GetTimestamp (Stream i s t p c) = t
  type SetTimestamp (Stream i s t p c) t' = Stream i s t' p c
  timestamp = stream . timestamp

instance EachFrameContent (Stream i s t p c) (Stream i s t p c') where
  type FrameContentFrom (Stream i s t p c) = c
  type FrameContentTo (Stream i s t p c') = c'
  eachFrameContent = stream . _Next . framePayload

instance (Default c, Default s, Default t) =>
         Default (Stream i s t p c) where
  def = MkStream (Next (MkFrame def def def))

instance (Show i, Show s, Show t, Show c, Show p) =>
         Show (Stream i s t p c) where
  show (MkStream s) = show s
