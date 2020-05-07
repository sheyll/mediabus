-- | A conduit that conceals 'Missing' frames.
module Data.MediaBus.Conduit.Discontinous
  ( concealMissing,
  )
where

import Conduit
import Control.Parallel.Strategies (NFData)
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Blank
import Data.MediaBus.Media.Discontinous
import Data.MediaBus.Media.Stream

-- | Replace 'Missing' parts with the given concealment value.
concealMissing ::
  forall c m i s t p.
  (NFData c, CanBeBlank c, Monad m) =>
  ConduitT (Stream i s t p (Discontinous c)) (Stream i s t p c) m ()
concealMissing = mapFrameContentC' go
  where
    go (Got !b) = b
    go Missing = blank @c -- TODO delete ??
