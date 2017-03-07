-- | Value level 'Ord'er. Sometimes values stored in e.g. 'Data.Set.Set's have
-- an 'Ord' instance, which does not represent the desired order. In that case
-- use this product type to pass a value along with the value to be for
-- comparison by the 'Eq' and 'Ord' instances.
module Data.MediaBus.Basics.OrderedBy ( OrderedBy(..) ) where

-- | A wrapper around a /payload/ value paired with a value to be used when /comparing/ that payload value.
data OrderedBy cmp a = MkOrderedBy
  { orderedByComparableValue :: cmp -- ^ Value to compare
  , orderedByValue :: a -- ^ actual value
  }

instance Eq cmp => Eq (OrderedBy cmp a) where
  MkOrderedBy cmpa _ == MkOrderedBy cmpb _ = cmpa == cmpb

instance Ord cmp => Ord (OrderedBy cmp a) where
  MkOrderedBy cmpa _ `compare` MkOrderedBy cmpb _ = cmpa `compare` cmpb
