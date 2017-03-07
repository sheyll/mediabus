-- | Discontinous content. Some media streams, like e.g. RTP streams received
-- via UDP, have the characteristic, that some times packages are lost. This
-- module provides a type that is isomorphic to 'Maybe' to indicate that the
-- content for a media frame is missing.
-- It might take some effort to detect missing content, like a jitter buffer, or the comparison of sequence numbers.
--
--  TODO: Create a gap detection mechanism, a simple stateful monad that knows the next timestamp etc.
module Data.MediaBus.Media.Discontinous
  ( Discontinous(..)
  , _Missing
  , _Got
  ) where

import Control.Lens
import Control.Parallel.Strategies (NFData)
import Data.Default
import Data.MediaBus.Media.Media
import Data.MediaBus.Media.Channels
import Data.MediaBus.Media.Samples
import Data.MediaBus.Basics.Ticks
import GHC.Generics (Generic)

-- | Content that can be 'Missing'.
data Discontinous a
  = Missing -- ^ A place holder for frame content that is missing
  | Got !a -- ^ Available content
  deriving (Show, Generic)

instance NFData a =>
         NFData (Discontinous a)

instance Default (Discontinous a) where
  def = Missing

makePrisms ''Discontinous

instance (HasDuration a) =>
         HasDuration (Discontinous a) where
  getDuration Missing = 0
  getDuration (Got !x) = getDuration x

instance EachMedia a b =>
         EachMedia (Discontinous a) (Discontinous b) where
  type MediaFromE (Discontinous a) = MediaFromE a
  type MediaToE (Discontinous b) = MediaToE b
  eachMedia = _Got . eachMedia

instance EachSample a b =>
         EachSample (Discontinous a) (Discontinous b) where
  type SamplesFrom (Discontinous a) = SamplesFrom a
  type SamplesTo (Discontinous b) = SamplesTo b
  eachSample = _Got . eachSample

instance EachChannel a b =>
         EachChannel (Discontinous a) (Discontinous b) where
  type ChannelsFrom (Discontinous a) = ChannelsFrom a
  type ChannelsTo (Discontinous b) = ChannelsTo b
  eachChannel = _Got . eachChannel

instance (HasRate c) =>
         HasRate (Discontinous c) where
  type GetRate (Discontinous c) = GetRate c
  type SetRate (Discontinous c) r' = Discontinous (SetRate c r')

instance ( HasRate i
         , GetRate i ~ ri
         , SetRate i rj ~ j
         , KnownRate rj
         , CoerceRate i j ri rj
         ) =>
         CoerceRate (Discontinous i) (Discontinous j) ri rj where
  coerceRate px = over _Got (coerceRate px)
