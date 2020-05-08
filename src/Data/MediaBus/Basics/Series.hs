-- | A series with a start value and consecutive next vaules.
module Data.MediaBus.Basics.Series
  ( Series (..),
    _Next,
    _Start,
    type Series',
    AsSeries (..),
    AsSeriesStart (..),
    AsSeriesNext (..),
  )
where

import Control.DeepSeq
import Control.Lens
import Data.Bifunctor
import GHC.Generics (Generic)
import Test.QuickCheck

-- | A value of a series is either the 'Start' of that series or the 'Next'
-- value in a started series.
data Series start element
  = Next {_seriesValue :: !element}
  | Start {_seriesStartValue :: !start}
  deriving (Eq, Generic)

makePrisms ''Series

-- | A simple version of a series, where the 'Start' value has the same type as
-- the 'Next' value.
type Series' element = Series element element

instance
  (NFData start, NFData elem) =>
  NFData (Series start elem)

instance
  (Show start, Show elem) =>
  Show (Series start elem)
  where
  showsPrec d (Start !x) =
    showParen (d > 10) $ showString "start: " . showsPrec 11 x
  showsPrec d (Next !x) =
    showParen (d > 10) $ showString "next: " . showsPrec 11 x

instance
  (Ord start, Ord elem) =>
  Ord (Series start elem)
  where
  compare (Next !l) (Next !r) = compare l r
  compare _ _ = EQ

instance
  (Arbitrary start, Arbitrary elem) =>
  Arbitrary (Series start elem)
  where
  arbitrary = do
    isNext <- choose (0.0, 1.0)
    if isNext < (0.95 :: Double)
      then Next <$> arbitrary
      else Start <$> arbitrary

instance Functor (Series start) where
  fmap = over _Next

instance Bifunctor Series where
  first = over _Start
  second = over _Next

-- | A class of types with /start/ and /next/ semantics,
-- just like 'Series'.
class AsSeries a start elem | a -> start, a -> elem where
  -- | A simple 'Prim' to extract a /start/ value
  seriesStart' :: Prism' a start

  -- | A simple 'Prim' to extract a /next/ value
  seriesNext' :: Prism' a elem

instance AsSeries (Either start elem) start elem where
  seriesStart' = _Left
  seriesNext' = _Right

instance AsSeries (Series start elem) start elem where
  seriesNext' = _Next
  seriesStart' = _Start

-- | A type class for types that might have a /start/ value.
class
  (SetSeriesStart s (GetSeriesStart s) ~ s) =>
  AsSeriesStart s where
  type GetSeriesStart s
  type SetSeriesStart s t

  -- | A 'Prism' for /start/ values
  seriesStart :: Prism s (SetSeriesStart s n) (GetSeriesStart s) n

instance AsSeriesStart (Either a b) where
  type GetSeriesStart (Either a b) = a
  type SetSeriesStart (Either a b) n = (Either n b)
  seriesStart = _Left

instance AsSeriesStart (Series a b) where
  type GetSeriesStart (Series a b) = a
  type SetSeriesStart (Series a b) n = (Series n b)
  seriesStart = _Start

-- | A type class for types that might have a /next/ value.
class
  (SetSeriesNext s (GetSeriesNext s) ~ s) =>
  AsSeriesNext s where
  type GetSeriesNext s
  type SetSeriesNext s t

  -- | A 'Prism' for the /next/ values
  seriesNext :: Prism s (SetSeriesNext s n) (GetSeriesNext s) n

instance AsSeriesNext (Either a b) where
  type GetSeriesNext (Either a b) = b
  type SetSeriesNext (Either a b) n = (Either a n)
  seriesNext = _Right

instance AsSeriesNext (Series a b) where
  type GetSeriesNext (Series a b) = b
  type SetSeriesNext (Series a b) n = (Series a n)
  seriesNext = _Next
