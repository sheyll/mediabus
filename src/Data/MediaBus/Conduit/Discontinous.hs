-- | A conduit that conceals 'Missing' frames.
module Data.MediaBus.Conduit.Discontinous
  ( concealMissing,
  )
where

import Conduit
import Control.Parallel.Strategies (NFData)
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream

-- | Replace 'Missing' parts with the given concealment value.
concealMissing ::
  (NFData c, Monad m) =>
  c ->
  ConduitT (Stream i s t p (Discontinous c)) (Stream i s t p c) m ()
concealMissing conceal = mapFrameContentC' go
  where
    go (Got !b) = b
    go Missing = conceal -- TODO delete ??
