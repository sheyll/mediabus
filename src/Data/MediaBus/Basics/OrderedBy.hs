-- | Value level 'Ord'er. Sometimes values are stored in e.g. 'Data.Set.Set's
-- and the default 'Ord' instance isn't what we want. In that case it would be
-- great to pass the comparison as an explicit function.
module Data.MediaBus.Basics.OrderedBy ( OrderedBy(..) ) where

-- | A wrapper around a value (to be compared) and a function to the value that
-- shall be compared against.
data OrderedBy cmp a = MkOrderedBy
  { orderedByComparableValue :: cmp
  , orderedByValue :: a
  }

instance Eq cmp => Eq (OrderedBy cmp a) where
  MkOrderedBy cmpa _ == MkOrderedBy cmpb _ = cmpa == cmpb

instance Ord cmp => Ord (OrderedBy cmp a) where
  MkOrderedBy cmpa _ `compare` MkOrderedBy cmpb _ = cmpa `compare` cmpb
