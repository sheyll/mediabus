module Data.MediaBus.Basics.OrderedBy ( OrderedBy(..) ) where

import Data.Function (on)

data OrderedBy a = MkOrderedBy { orderedByCompare :: a -> a -> Ordering
                              , orderedByValue   :: a
                              }

instance Eq (OrderedBy a) where
    l == r = EQ == ((orderedByCompare l `on` orderedByValue) l r)

instance Ord (OrderedBy a) where
    l `compare` r = (orderedByCompare l `on` orderedByValue) l r
