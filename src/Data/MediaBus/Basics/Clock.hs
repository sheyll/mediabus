-- | An interface for /clocks/. Since there are many ways to calculate different
-- ideas of /time/, e.g. system time, wall clock time or stream time, a type
-- class 'Clock' is provided with the basic interface for getting the current
-- time and comparing time stamps.
module Data.MediaBus.Basics.Clock
  ( IsClock(..)
  , timeSince
  , UtcClock(..)
  , useUtcClock
  , utcClockTimeDiff
  ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Function (on)
import Data.Kind
import Data.MediaBus.Basics.Monotone
import Data.Proxy
import Data.Time.Calendar
import Data.Time.Clock
import Data.Word
import GHC.Generics (Generic)
import Test.QuickCheck

-- * Clocks
-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class ( Default (ClockTimeDiff c)
      , Ord (ClockTimeDiff c)
      , Eq (ClockTimeDiff c)
      , Num (ClockTimeDiff c)
      , Show (ClockTime c)
      , Eq (ClockTime c)
      , Show (ClockTimeDiff c)
      , LocalOrd (ClockTimeDiff c)
      ) =>
      IsClock c where
  data ClockTime c
  data ClockTimeDiff c
  type MonadClock c (m :: Type -> Type) :: Constraint
  -- | Return the current absolute time as a 'ClockTime'
  now
    :: MonadClock c m
    => m (ClockTime c)
  -- | Convert a 'ClockTime' to a 'ClockTimeDiff' by 'diffTime'ing the time
  -- with the beginning of time.
  timeAsTimeDiff :: ClockTime c -> ClockTimeDiff c
  -- | The time difference (a 'ClockTimeDiff') between two 'ClockTime's
  diffTime :: ClockTime c -> ClockTime c -> ClockTimeDiff c
  -- | Add the relative time to an absolute time to obtain a new absolute
  -- time.
  timeAddTimeDiff :: ClockTime c -> ClockTimeDiff c -> ClockTime c

-- | Return the time elapsed between since the given absolute time.
timeSince
  :: (IsClock c, MonadClock c m, Monad m)
  => ClockTime c -> m (ClockTimeDiff c)
timeSince t0 = do
  t1 <- now
  return (diffTime t1 t0)

-- * UTC System Time Clock
-- | A UTC System Time Clock
data UtcClock =
  MkUtcClock
  deriving (Generic)

instance NFData UtcClock

-- | A 'Proxy' for 'UtcClock'
useUtcClock :: Proxy UtcClock
useUtcClock = Proxy

instance IsClock UtcClock where
  newtype ClockTime UtcClock = MkUtcClockTime{_utcClockTime ::
                                            UTCTime}
                           deriving (Eq, Generic)
  newtype ClockTimeDiff
          UtcClock = MkUtcClockTimeDiff{_utcClockTimeDiff :: NominalDiffTime}
                   deriving (Ord, Eq, Num, Generic)
  type MonadClock UtcClock m = MonadIO m
  now = MkUtcClockTime <$> liftIO getCurrentTime
  timeAsTimeDiff (MkUtcClockTime ref) =
    MkUtcClockTimeDiff $ diffUTCTime ref $ UTCTime (toEnum 0) 0
  timeAddTimeDiff (MkUtcClockTime t) (MkUtcClockTimeDiff dt) =
    MkUtcClockTime (addUTCTime dt t)
  diffTime (MkUtcClockTime later) (MkUtcClockTime sooner) =
    MkUtcClockTimeDiff $ diffUTCTime later sooner

instance NFData (ClockTime UtcClock)

instance Show (ClockTime UtcClock) where
  show (MkUtcClockTime !t) = show t

instance Show (ClockTimeDiff UtcClock) where
  showsPrec d (MkUtcClockTimeDiff t) =
    showParen (d > 10) $ showString "Δt " . showsPrec 11 t

instance NFData (ClockTimeDiff UtcClock)

instance Default (ClockTimeDiff UtcClock) where
  def = MkUtcClockTimeDiff $ fromInteger def

instance Arbitrary (ClockTime UtcClock) where
  arbitrary =
    MkUtcClockTime <$>
    (UTCTime <$> (ModifiedJulianDay <$> arbitrary) <*>
     (fromInteger <$> arbitrary))

instance Arbitrary (ClockTimeDiff UtcClock) where
  arbitrary = MkUtcClockTimeDiff . fromInteger <$> arbitrary

-- | An 'Iso' for converting a 'UtcClock' 'ClockTimeDiff' from/to a 'NominalDiffTime'.
utcClockTimeDiff :: Iso' (ClockTimeDiff UtcClock) NominalDiffTime
utcClockTimeDiff = iso _utcClockTimeDiff MkUtcClockTimeDiff

instance LocalOrd (ClockTimeDiff UtcClock) where
  succeeds = succeeds `on` roundToSeconds
    where
      roundToSeconds = round . (/ 1000000000000) . _utcClockTimeDiff
      roundToSeconds :: ClockTimeDiff UtcClock -> Word64
