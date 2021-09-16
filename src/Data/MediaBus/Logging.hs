-- | Functions for users of this library to
-- filter out log messages from mediabus.
module Data.MediaBus.Logging (isMediaBusLogSource) where

import Data.MediaBus.InternalLogging (myLogSource)
import Control.Monad.Logger (LogSource)

-- | A predicate for 'LogSource's matching logging
-- from this library.
isMediaBusLogSource :: LogSource -> Bool
isMediaBusLogSource = (== myLogSource)
