-- | Logging utilities for 'Conduit's.
module Data.MediaBus.Conduit.Logging
  ( prefixLogsC
  )
where

import           Control.Monad.Logger
import           Data.Conduit

-- | Prefix all log messages of the given 'Conduit' with a 'Text'.
-- This is similar to 'Data.MediaBus.Basisc.LoggingExtra.withLogMessagePrefix'.
prefixLogsC
  :: (ToLogStr prefix, MonadLoggerIO m)
  => prefix
  -> ConduitT i o (LoggingT m) r
  -> ConduitT i o m r
prefixLogsC prefix nested = do
  originalLogger <- askLoggerIO
  transPipe (`runLoggingT` wrapLogger originalLogger) nested
 where
  wrapLogger originalLogger loc src lvl msg =
    originalLogger loc src lvl (toLogStr prefix <> msg)
