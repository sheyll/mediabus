module Data.MediaBus.Discontinous
  ( Discontinous(..)
  , _Missing
  , _Got
  , concealMissing
  ) where

import Conduit
import Control.Lens
import Control.Parallel.Strategies (NFData)
import Data.Default
import Data.MediaBus.Media
import Data.MediaBus.Media.Channels
import Data.MediaBus.Media.Samples
import Data.MediaBus.Stream
import Data.MediaBus.Ticks
import GHC.Generics (Generic)

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
data Discontinous a
  = Missing
  | Got !a
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

concealMissing
  :: (NFData c, Monad m)
  => c -> Conduit (Stream i s t p (Discontinous c)) m (Stream i s t p c)
concealMissing conceal = mapPayloadC' go
  where
    go (Got !b) = b
    go Missing = conceal -- TODO delete ??
