module Data.MediaBus.Conduit
    ( annotateTypeC
    , annotateTypeCIn
    , annotateTypeCOut
    , annotateTypeSink
    , annotateTypeSource
    , exitAfterC
    , traceShowC
    , traceShowSink
    ) where

import           Conduit
import           Data.Conduit.List
import           Control.Monad.State.Strict as State
import           Debug.Trace
import           System.Random

annotateTypeC :: proxy a -> Conduit a m a -> Conduit a m a
annotateTypeC _ = id

annotateTypeCIn :: proxy a -> Conduit a m b -> Conduit a m b
annotateTypeCIn _ = id

annotateTypeCOut :: proxy b -> Conduit a m b -> Conduit a m b
annotateTypeCOut _ = id

annotateTypeSource :: proxy a -> Source m a -> Source m a
annotateTypeSource _ = id

annotateTypeSink :: proxy a -> Sink a m r -> Sink a m r
annotateTypeSink _ = id

exitAfterC :: Monad m => Int -> Conduit a m a
exitAfterC 0 = return ()
exitAfterC n = await >>= maybe (return ()) (yield >=> const (exitAfterC (n - 1)))

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

traceShowSink :: (Show a, Monad m) => Double -> String -> Consumer a m [a]
traceShowSink probability msg =
    traceShowC probability msg .| consume
