module Data.MediaBus.InternalLogging (myLogSource, wrn, dbg, out, err) where

import Control.Monad.Logger
import Data.String (IsString (fromString))

myLogSource :: LogSource
myLogSource = "mediabus"

err :: MonadLogger m => String -> m ()
err = myLog LevelError

out :: MonadLogger m => String -> m ()
out = myLog LevelInfo

dbg :: MonadLogger m => String -> m ()
dbg = myLog LevelDebug

wrn :: MonadLogger m => String -> m ()
wrn = myLog LevelWarn

myLog :: MonadLogger m => LogLevel -> String -> m ()
myLog l = logOtherNS myLogSource l . fromString