module Data.MediaBus.Basics.Monotone
  ( LocalOrd (..),
  )
where

import Data.Int
import Data.Word

-- | Class of numbers that are monotone increasing having only a relative order,
-- that is not necessarily transitive.
--
-- For example, a series of 'Word8' values: @0 64 128 192 0 64 128 ...@ could be
-- interpreted as a monotone series of consecutive increasing values, that wrap
-- around after 255. But note that the 'Ord' instance is not sufficient to
-- express that @0@ `succeeds` @192@, since 'Ord' ensures complete transitivity
-- and therefore @0 < 192@.
class LocalOrd a where
  succeeds :: a -> a -> Bool

instance LocalOrd Word8 where
  x `succeeds` y =
    let d = x - y
     in d > 0 && d <= ((maxBound - minBound) `div` 2)

instance LocalOrd Word16 where
  x `succeeds` y =
    let d = x - y
     in d > 0 && d <= ((maxBound - minBound) `div` 2)

instance LocalOrd Word32 where
  x `succeeds` y =
    let d = x - y
     in d > 0 && d <= ((maxBound - minBound) `div` 2)

instance LocalOrd Word64 where
  x `succeeds` y =
    let d = x - y
     in d > 0 && d <= ((maxBound - minBound) `div` 2)

instance LocalOrd Int8 where
  x `succeeds` y = x - y > 0

instance LocalOrd Int16 where
  x `succeeds` y = x - y > 0

instance LocalOrd Int32 where
  x `succeeds` y = x - y > 0

instance LocalOrd Int64 where
  x `succeeds` y = x - y > 0

instance LocalOrd Int where
  x `succeeds` y = x - y > 0
