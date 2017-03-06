-- | Conduits to create and convert the time stamps in 'Stream's
module Data.MediaBus.Stream.Timing
  ( convertTicksC'
  , deriveFrameTimestamp
  ) where

import Conduit
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad.State.Strict
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Stream
import Data.MediaBus.Stream

-- | Overwrite the timestamp of a stream of things that  have a time stamp field
--  (i.e. 'HasTimestamp' instances)  and also a duration, such that the
--  timestamps increment by the duration starting from 0.
deriveFrameTimestamp
  :: forall m r t a.
     (Monad m, CanBeTicks r t, HasDuration a, HasTimestamp a)
  => Ticks r t -> Conduit a m (SetTimestamp a (Ticks r t))
deriveFrameTimestamp t0 = evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync :: a
              -> Conduit a (StateT (Ticks r t) m) (SetTimestamp a (Ticks r t))
    yieldSync sb = do
      t <- get
      modify (+ (nominalDiffTime # getDuration sb))
      yield (sb & timestamp .~ t)

-- | Recalculate all timestamps in a 'Stream'
convertTicksC'
  :: forall proxy0 proxy1 m r t r' t' i s c p.
     (NFData t, NFData t', CanBeTicks r t, CanBeTicks r' t', Monad m, NFData t')
  => proxy0 '( r, t)
  -> proxy1 '( r', t')
  -> Conduit (Stream i s (Ticks r t) p c) m (Stream i s (Ticks r' t') p c)
convertTicksC' _ _ = mapTicksC' convertTicks
