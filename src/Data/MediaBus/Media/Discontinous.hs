-- | Streams with gaps.
--
-- Some media streams are transported via unreliable connections.
-- For example RTP streams received via UDP.
--
-- The transport sometimes loose packages.
--
-- This module provides a type that is isomorphic to 'Maybe' to indicate that the
-- content for a media frame is missing.
--
-- It might take some effort to detect missing content,
-- for example a jitter buffer, that compares sequence numbers.
--
-- This, however is not part of this module.
--
-- TODO: Create a gap detection mechanism, a simple stateful monad that knows the next timestamp etc.
module Data.MediaBus.Media.Discontinous
  ( Discontinous (..),
    _Missing,
    _Got,
    StreamWithGaps,
  )
where

import Control.Lens (makePrisms, over)
import Control.Parallel.Strategies (NFData)
import Data.Default (Default (..))
import Data.MediaBus.Basics.Ticks
  ( CoerceRate (..),
    HasDuration (getDuration),
    HasRate (..),
    HasStaticDuration (..),
    KnownRate,
  )
import Data.MediaBus.Media.Channels (EachChannel (..))
import Data.MediaBus.Media.Media (EachMedia (..))
import Data.MediaBus.Media.Samples (EachSample (..))
import Data.MediaBus.Media.Stream (Stream)
import GHC.Generics (Generic)

-- | Content that can be 'Missing'.
--
-- This is usually used in a 'Stream' as a wrapper for the 'Frame' payload.
-- For example:
-- > fooWithGaps :: Stream SourceId SeqNum Timestamp32 FooCodecInit (Discontinous FooCodecPayload)
data Discontinous a
  = -- | A place holder for frame content that is missing
    Missing
  | -- | Available content
    Got !a
  deriving (Show, Generic, Eq, Ord, Functor)

instance
  NFData a =>
  NFData (Discontinous a)

instance Default (Discontinous a) where
  def = Missing

makePrisms ''Discontinous

instance
  (HasDuration a) =>
  HasDuration (Discontinous a)
  where
  getDuration Missing = 0
  getDuration (Got !x) = getDuration x

instance
  EachMedia a b =>
  EachMedia (Discontinous a) (Discontinous b)
  where
  type MediaFromE (Discontinous a) = MediaFromE a
  type MediaToE (Discontinous b) = MediaToE b
  eachMedia = _Got . eachMedia

instance
  EachSample a b =>
  EachSample (Discontinous a) (Discontinous b)
  where
  type SamplesFrom (Discontinous a) = SamplesFrom a
  type SamplesTo (Discontinous b) = SamplesTo b
  eachSample = _Got . eachSample

instance
  EachChannel a b =>
  EachChannel (Discontinous a) (Discontinous b)
  where
  type ChannelsFrom (Discontinous a) = ChannelsFrom a
  type ChannelsTo (Discontinous b) = ChannelsTo b
  eachChannel = _Got . eachChannel

instance
  (HasRate c) =>
  HasRate (Discontinous c)
  where
  type GetRate (Discontinous c) = GetRate c
  type SetRate (Discontinous c) r' = Discontinous (SetRate c r')

instance
  ( HasRate i,
    GetRate i ~ ri,
    SetRate i rj ~ j,
    KnownRate rj,
    CoerceRate i j ri rj
  ) =>
  CoerceRate (Discontinous i) (Discontinous j) ri rj
  where
  coerceRate px = over _Got (coerceRate px)

instance
  (HasStaticDuration c) =>
  HasStaticDuration (Discontinous c)
  where
  type SetStaticDuration (Discontinous c) pt = Discontinous (SetStaticDuration c pt)
  type GetStaticDuration (Discontinous c) = GetStaticDuration c

-- | A 'Stream' that has 'Discontinous' payloads.
type StreamWithGaps i s t p c = Stream i s t p (Discontinous c)
