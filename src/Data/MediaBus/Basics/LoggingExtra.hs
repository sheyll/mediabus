-- | Utilities for logging.
module Data.MediaBus.Basics.LoggingExtra
  ( withLogMessagePrefix
  ) where

import Control.Monad.Logger

-- | Prefix every log message done from inside the given monad action with the
-- given prefix. This runs a 'LoggingT' action inside a 'MonadLoggerIO' base
-- monad with a log function that wraps around the log function returned by
-- 'askLoggerIO' and prefixes each message with the given prefix.
withLogMessagePrefix
  :: (ToLogStr prefix, MonadLoggerIO m)
  => prefix -> LoggingT m a -> m a
withLogMessagePrefix prefix ml = do
  originalLogger <- askLoggerIO
  let logger loc lvl src msg =
        originalLogger loc lvl src (prefixL `mappend` msg)
      prefixL = toLogStr prefix
  runLoggingT ml logger
