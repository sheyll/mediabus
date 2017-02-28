module Data.MediaBus.Series
    ( Series(..)
    , type Series'
    , _Next
    , _Start
    , AsSeries(..)
    , AsSeriesStart(..)
    , AsSeriesNext(..)
    , StartingFrom(..)
    , startingFromValue
    , toNextsC'
    , toStartsC'
    , monotoneSeriesC
    ) where

import           Control.Lens
import           Conduit
import           Control.Monad.Reader
import           Test.QuickCheck
import           Data.Bifunctor
import           GHC.Generics         ( Generic )
import           Control.DeepSeq

class (SetSeriesStart s (GetSeriesStart s) ~ s) =>
      AsSeriesStart s where
    type GetSeriesStart s
    type SetSeriesStart s t
    seriesStart :: Prism s (SetSeriesStart s n) (GetSeriesStart s) n

class (SetSeriesNext s (GetSeriesNext s) ~ s) =>
      AsSeriesNext s where
    type GetSeriesNext s
    type SetSeriesNext s t
    seriesNext :: Prism s (SetSeriesNext s n) (GetSeriesNext s) n

class AsSeries s a b | s -> a, s -> b where
    seriesStart' :: Prism' s a
    seriesNext' :: Prism' s b

instance AsSeries (Either a b) a b where
    seriesStart' = _Left
    seriesNext' = _Right

instance AsSeriesStart (Either a b) where
    type GetSeriesStart (Either a b) = a
    type SetSeriesStart (Either a b) n = (Either n b)
    seriesStart = _Left

instance AsSeriesNext (Either a b) where
    type GetSeriesNext (Either a b) = b
    type SetSeriesNext (Either a b) n = (Either a n)
    seriesNext = _Right

data Series a b = Next { _seriesValue :: !b }
                | Start { _seriesStartValue :: !a }
    deriving (Eq, Generic)

instance (NFData a, NFData b) =>
         NFData (Series a b)

instance (Show a, Show b) =>
         Show (Series a b) where
    show (Start !x) = "(START: " ++ show x ++ ")"
    show (Next !x) = show x

instance (Ord a, Ord b) =>
         Ord (Series a b) where
    compare (Next !l) (Next !r) =
        compare l r
    compare _ _ = EQ

type Series' a = Series a a

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Series a b) where
    arbitrary = do
        isNext <- choose (0.0, 1.0)
        if isNext < (0.95 :: Double)
            then Next <$> arbitrary
            else Start <$> arbitrary

makePrisms ''Series

instance AsSeries (Series a b) a b where
    seriesNext' = _Next
    seriesStart' = _Start

instance AsSeriesNext (Series a b) where
    type GetSeriesNext (Series a b) = b
    type SetSeriesNext (Series a b) n = (Series a n)
    seriesNext = _Next

instance AsSeriesStart (Series a b) where
    type GetSeriesStart (Series a b) = a
    type SetSeriesStart (Series a b) n = (Series n b)
    seriesStart = _Start

instance Functor (Series a) where
    fmap = over _Next

instance Bifunctor Series where
    first = over _Start
    second = over _Next

newtype StartingFrom a = MkStartingFrom { _startingFromValue :: a }
    deriving (Eq, Ord, Arbitrary)

makeLenses ''StartingFrom

instance Show a =>
         Show (StartingFrom a) where
    show (MkStartingFrom !x) =
        "(STARTING-FROM: " ++ show x ++ ")"

toNextsC' :: Monad m => Conduit (Series a b) m b
toNextsC' = awaitForever go
  where
    go (Start !_a) = return ()
    go (Next !b) = yield b

toStartsC' :: Monad m => Conduit (Series a b) m a
toStartsC' = awaitForever go
  where
    go (Start !a) = yield a
    go (Next !_b) = return ()

monotoneSeriesC :: Monad m => m a -> (i -> m b) -> Conduit i m (Series a b)
monotoneSeriesC !initSeries !continueSeries = do
    !rStart <- lift initSeries
    yield (Start rStart)
    !mi <- await
    mapM_ (lift . continueSeries >=>
               yield . Next >=>
                   const (awaitForever (lift . continueSeries >=> yield . Next)))
          mi
