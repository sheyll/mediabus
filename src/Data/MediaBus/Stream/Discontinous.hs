-- | A conduit that conceals 'Missing' frames.
module Data.MediaBus.Stream.Discontinous
  ( concealMissing
  ) where

import Conduit
import Control.Parallel.Strategies (NFData)
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream
import Data.MediaBus.Stream

-- | Replace 'Missing' parts with the given concealment value.
concealMissing
  :: (NFData c, Monad m)
  => c -> Conduit (Stream i s t p (Discontinous c)) m (Stream i s t p c)
concealMissing conceal = mapFrameContentC' go
  where
    go (Got !b) = b
    go Missing = conceal -- TODO delete ??
