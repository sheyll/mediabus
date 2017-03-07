-- | Probabilistic tracing conduits using 'Debug.Trace'
module Data.MediaBus.Conduit.Trace
    ( exitAfterC
    , traceShowC
    , traceShowSink
    ) where

import           Conduit
import           Data.Conduit.List
import           Control.Monad.State.Strict as State
import           Debug.Trace
import           System.Random

-- | Receive and trace the 'Show'n value prefixed by a message and then yield
-- down the stream. Since the number of elements sent over a conduit can be
-- tremendous, a tracing probability parameter was added, that can be between
-- @0.0@ - @1.0@, and sets the probability of with which a message will be
-- traced. This uses 'Debug.Trace.traceM' internally. Do not use this for
-- logging.
traceShowC :: (Show a, Monad m) => Double -> String -> Conduit a m a
traceShowC probability msg =
    evalStateC (mkStdGen 100, 0 :: Integer) $
        awaitForever $
        \x -> do
            (g, omitted) <- State.get
            let (p, g') = randomR (0, 1) g
            if p < probability
                then do
                    let omittedmsg = if omitted == 0
                                     then ""
                                     else " *** " ++
                                         show omitted ++
                                         " messages omitted"
                    traceM ((if null msg then "" else msg ++ ": ") ++ show x ++
                                omittedmsg)
                    State.put (g', 0)
                else State.put (g', omitted + 1)
            yield x

-- | Like 'traceShowC' but implemented as a 'Consumer' that also returns all
-- received inputs as a list when the conduit terminates.
traceShowSink :: (Show a, Monad m) => Double -> String -> Consumer a m [a]
traceShowSink probability msg =
    traceShowC probability msg .| consume

-- | For profiling and debugging it can sometimes be useful, to have a circuit
-- breaker, like this conduit, that exists after an given number of items have
-- been processed, and, until then just yields all inputs.
exitAfterC :: Monad m => Int -> Conduit a m a
exitAfterC 0 = return ()
exitAfterC n = await >>= maybe (return ()) (yield >=> const (exitAfterC (n - 1)))
