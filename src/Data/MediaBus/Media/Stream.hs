-- | A media 'Stream' is often represented by a 'Series' of related 'Frame's
-- with a 'FrameCtx' created when the 'Stream' 'Start's. This module contains
-- the basic definitions of the building blocks of media content that is create,
-- processed, and presented by a possibly infinite 'Series' of chunks called
-- 'Frame's.
module Data.MediaBus.Media.Stream
  ( FrameCtx (..),
    frameCtxSourceId,
    frameCtxSeqNumRef,
    frameCtxTimestampRef,
    frameCtxInit,
    EachFrameCtxInit (..),
    Frame (..),
    EachFramePayload (..),
    frameSeqNum,
    frameTimestamp,
    framePayload,
    Stream (..),
    type Streamish,
    stream,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Default
import Data.MediaBus.Basics.Sequence
import Data.MediaBus.Basics.Series
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Channels
import Data.MediaBus.Media.Media
import GHC.Generics (Generic)
import Test.QuickCheck

-- | Meta information about a media 'Stream'.
data FrameCtx sourceId sequenceNumber timestamp initSegment
  = MkFrameCtx
      { -- | An identifier for the stream, e.g. a track name
        -- or an RTP SSRC
        _frameCtxSourceId :: !sourceId,
        -- | The start time stamp of a stream all time
        -- stamps in the 'Frame's are relative to this.
        _frameCtxTimestampRef :: !timestamp,
        -- | The start sequence number of a stream all
        -- sequence numbers in the 'Frame's are relative to
        -- this.
        _frameCtxSeqNumRef :: !sequenceNumber,
        -- | An extra field for media type specific extra
        -- information, like the init segment of an ISOBMFF
        -- media.
        _frameCtxInit :: !initSegment
      }
  deriving (Eq, Ord, Generic)

instance
  (NFData i, NFData s, NFData t, NFData p) =>
  NFData (FrameCtx i s t p)

makeLenses ''FrameCtx

-- | Class for types that have a 'Traversal' for '_frameCtxInit'
class EachFrameCtxInit s t where
  type FrameCtxInitFrom s
  type FrameCtxInitTo t

  -- | A traversal for the 'FrameCtx' initial content, skipping over any 'Frame'
  eachFrameCtxInit :: Traversal s t (FrameCtxInitFrom s) (FrameCtxInitTo t)

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

instance
  (Arbitrary i, Arbitrary s, Arbitrary t, Arbitrary p) =>
  Arbitrary (FrameCtx i s t p)
  where
  arbitrary = MkFrameCtx <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance
  (Default i, Default s, Default t, Default p) =>
  Default (FrameCtx i s t p)
  where
  def = MkFrameCtx def def def def

instance
  (Show i, Show s, Show t, Show p) =>
  Show (FrameCtx i s t p)
  where
  showsPrec d (MkFrameCtx sid tsr snr spr) =
    showParen (d > 10) $
      showString "frame context: "
        . showsPrec 11 sid
        . showChar ' '
        . showsPrec 11 snr
        . showChar ' '
        . showsPrec 11 tsr
        . showChar ' '
        . showsPrec 11 spr

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
--
-- A 'Frame' can hold a 'Data.MediaBus.Media.Segment'
data Frame s t c
  = MkFrame
      { -- | The timestamp at which the representation of the
        -- media content in this frame **begins**
        _frameTimestamp :: !t,
        -- | The sequence number of that frame, useful to detect
        -- missing frames
        _frameSeqNum :: !s,
        -- | The actual (media) content.
        _framePayload :: !c
      }
  deriving (Eq, Ord, Generic)

instance
  (NFData c, NFData s, NFData t) =>
  NFData (Frame s t c)

deriving instance Functor (Frame s t)

makeLenses ''Frame

-- | Class for types that have a 'Traversal' for '_framePayload'
class EachFramePayload s t where
  type FramePayloadFrom s
  type FramePayloadTo t

  -- | A traversal for the frame content of all 'Frame's, skipping over any
  -- 'FrameCtx'
  eachFramePayload :: Traversal s t (FramePayloadFrom s) (FramePayloadTo t)

instance EachFramePayload (Frame s t c) (Frame s t c') where
  type FramePayloadFrom (Frame s t c) = c
  type FramePayloadTo (Frame s t c') = c'
  eachFramePayload = framePayload

instance
  (EachChannel c c') =>
  EachChannel (Frame s t c) (Frame s t c')
  where
  type ChannelsFrom (Frame s t c) = c
  type ChannelsTo (Frame s t c') = c'
  eachChannel = framePayload

instance
  (HasMedia c c') =>
  HasMedia (Frame s t c) (Frame s t c')
  where
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

instance
  HasDuration c =>
  HasDuration (Frame s t c)
  where
  getDuration = getDuration . _framePayload

instance
  HasStaticDuration c =>
  HasStaticDuration (Frame s t c)
  where
  type SetStaticDuration (Frame s t c) pt = Frame s t (SetStaticDuration c pt)
  type GetStaticDuration (Frame s t c) = GetStaticDuration c

instance
  (Arbitrary c, Arbitrary s, Arbitrary t) =>
  Arbitrary (Frame s t c)
  where
  arbitrary = MkFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance
  (Default s, Default t, Default c) =>
  Default (Frame s t c)
  where
  def = MkFrame def def def

instance
  (Show s, Show t, Show v) =>
  Show (Frame s t v)
  where
  showsPrec d (MkFrame ts sn v) =
    showParen (d > 10) $
      showString "frame: "
        . showsPrec 11 sn
        . showChar ' '
        . showsPrec 11 ts
        . showChar ' '
        . showsPrec 11 v

-- | A 'Series' of 'Frame's started by a
-- 'FrameCtx'. This combines the sum type 'Series' which has either 'Start' or
-- 'Next' with 'FrameCtx' and 'Frame'.
--
-- This a just a newtype wrapper around 'Streamish'
--
-- An example for a 'Stream' with the type parameters fully applied:
--
-- > Stream
-- >    RtpSsrc
-- >    RtpSeqNum
-- >    (Ticks (Hz 16000) Word64)
-- >    ()
-- >    (Segment
-- >       (640 :/ Hz 16000)
-- >       (Audio
-- >          (Hz 16000)
-- >          Mono
-- >          (Raw S16)))
newtype Stream i s t p c
  = MkStream
      { _stream :: Streamish i s t p c
      }
  deriving (Ord, Eq, Arbitrary, Generic)

instance
  (NFData i, NFData s, NFData t, NFData c, NFData p) =>
  NFData (Stream i s t p c)

-- | This is the type alias that 'Stream' is a newtype wrapper of, see the
-- description of 'Stream'. This combines the sum type 'Series' which has either 'Start' or
-- 'Next' with 'FrameCtx' and 'Frame'.
--
-- See 'Stream' for the newtype wrapper.
type Streamish i s t p c = Series (FrameCtx i s t p) (Frame s t c)

makeLenses ''Stream

instance
  EachChannel (Frame s t c) (Frame s t c') =>
  EachChannel (Stream i s t p c) (Stream i s t p c')
  where
  type ChannelsFrom (Stream i s t p c) = ChannelsFrom (Frame s t c)
  type ChannelsTo (Stream i s t p c') = ChannelsTo (Frame s t c')
  eachChannel = stream . _Next . eachChannel

instance
  HasDuration c =>
  HasDuration (Stream i s t p c)
  where
  getDuration = maybe 0 getDuration . preview (stream . _Next)

instance HasSeqNum (Stream i s t p c) where
  type GetSeqNum (Stream i s t p c) = s
  type SetSeqNum (Stream i s t p c) x = Stream i x t p c
  seqNum = stream . seqNum

instance HasTimestamp (Stream i s t p c) where
  type GetTimestamp (Stream i s t p c) = t
  type SetTimestamp (Stream i s t p c) t' = Stream i s t' p c
  timestamp = stream . timestamp

instance EachFramePayload (Stream i s t p c) (Stream i s t p c') where
  type FramePayloadFrom (Stream i s t p c) = c
  type FramePayloadTo (Stream i s t p c') = c'
  eachFramePayload = stream . _Next . framePayload

instance
  (Default c, Default s, Default t) =>
  Default (Stream i s t p c)
  where
  def = MkStream (Next (MkFrame def def def))

instance
  (Show i, Show s, Show t, Show c, Show p) =>
  Show (Stream i s t p c)
  where
  showsPrec d (MkStream s) = showsPrec d s
