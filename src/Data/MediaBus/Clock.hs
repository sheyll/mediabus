module Data.MediaBus.Clock
    ( IsClock(..)
    , timeSince
    , UtcClock(..)
    , useUtcClock
    , _utcClockTimeDiff
    , _utcClockTime
    , utcClockTimeDiff
    ) where

import           Conduit
import           Control.Lens
import           Data.Default
import           Data.Function                   ( on )
import           Data.Kind
import           Data.MediaBus.Monotone
import           Data.Proxy
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Word
import           Test.QuickCheck
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq

-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class (Default (ClockTimeDiff c), Ord (ClockTimeDiff c), Eq (ClockTimeDiff c), Num (ClockTimeDiff c), Show (ClockTime c), Eq (ClockTime c), Show (ClockTimeDiff c), LocalOrd (ClockTimeDiff c)) =>
      IsClock c where
    data ClockTime c
    data ClockTimeDiff c
    type MonadClock c (m :: Type -> Type) :: Constraint
    now :: MonadClock c m => m (ClockTime c)
    timeAsTimeDiff :: ClockTime c -> ClockTimeDiff c
    diffTime :: ClockTime c -> ClockTime c -> ClockTimeDiff c
    timeAddTimeDiff :: ClockTime c -> ClockTimeDiff c -> ClockTime c

timeSince :: (IsClock c, MonadClock c m, Monad m) => ClockTime c -> m (ClockTimeDiff c)
timeSince t0 = do
    t1 <- now
    return (diffTime t1 t0)

data UtcClock = MkUtcClock
    deriving Generic

instance NFData UtcClock

useUtcClock :: Proxy UtcClock
useUtcClock = Proxy

instance IsClock UtcClock where
    newtype ClockTime UtcClock = MkUtcClockTime{_utcClockTime :: UTCTime}
                      deriving (Eq, Generic)
    newtype ClockTimeDiff UtcClock = MkUtcClockTimeDiff{_utcClockTimeDiff ::
                                          NominalDiffTime}
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
    show (MkUtcClockTime t) = show t

instance Show (ClockTimeDiff UtcClock) where
    show (MkUtcClockTimeDiff t) =
        "dt:" ++ show t

instance NFData (ClockTimeDiff UtcClock)

instance Default (ClockTimeDiff UtcClock) where
    def = MkUtcClockTimeDiff $ fromInteger def

instance Arbitrary (ClockTime UtcClock) where
    arbitrary = MkUtcClockTime <$> (UTCTime <$> (ModifiedJulianDay <$> arbitrary)
                                       <*> (fromInteger <$> arbitrary))

instance Arbitrary (ClockTimeDiff UtcClock) where
    arbitrary = MkUtcClockTimeDiff . fromInteger <$> arbitrary

utcClockTimeDiff :: Lens' (ClockTimeDiff UtcClock) NominalDiffTime
utcClockTimeDiff = lens _utcClockTimeDiff (const MkUtcClockTimeDiff)

instance LocalOrd (ClockTimeDiff UtcClock) where
    succeeds = succeeds `on` roundToSeconds
      where
        roundToSeconds = round . (/ 1000000000000) . _utcClockTimeDiff
        roundToSeconds :: ClockTimeDiff UtcClock -> Word64
