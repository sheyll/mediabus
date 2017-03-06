module Data.MediaBus.Basics.Monotone
    ( LocalOrd(..)
    ) where

import           Data.Word
import           Data.Int

-- | Class of numbers that are monotone increasing (or decreasing) and have a
-- relative order, that is not necessarily transitive.
--
-- For example, for a series of 'Word8' values: @0 64 128 192 0 64 128 ...@
-- could be interpreted as a monotone series of consecutive increasing values,
-- that wrap around after 255. But note that the 'Ord' instance is not
-- sufficient to express that @0@ is __after__ @192@, since @0 < 192@.
class LocalOrd a where
    succeeds :: a -> a -> Bool
    default succeeds :: (Bounded a, Integral a) => a -> a -> Bool
    x `succeeds` y = (x - y) < ((maxBound - minBound) `div` 2)

instance LocalOrd Word8

instance LocalOrd Word16

instance LocalOrd Word32

instance LocalOrd Word64

instance LocalOrd Int8

instance LocalOrd Int16

instance LocalOrd Int32

instance LocalOrd Int64

instance LocalOrd Int
