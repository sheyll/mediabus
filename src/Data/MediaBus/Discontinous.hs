module Data.MediaBus.Discontinous
    ( Discontinous(..)
    , _Missing
    , _Got
    , concealMissing
    ) where

import           Conduit
import           Control.Lens
import           Control.Parallel.Strategies ( NFData )
import           Data.Default
import           Data.MediaBus.Ticks
import           Data.MediaBus.Payload
import           Data.MediaBus.Stream
import           GHC.Generics                ( Generic )

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
data Discontinous a = Missing
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

instance HasPayload a =>
         HasPayload (Discontinous a) where
    type GetPayload (Discontinous a) = GetPayload a
    type SetPayload (Discontinous a) b = Discontinous (SetPayload a b)
    payload = _Got . payload

concealMissing :: (NFData c, Monad m)
               => c
               -> Conduit (Stream i s t p (Discontinous c)) m (Stream i s t p c)
concealMissing conceal =
    mapPayloadC' go
  where
    go (Got !b) = b
    go Missing = conceal-- TODO delete ??
