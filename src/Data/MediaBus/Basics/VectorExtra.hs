-- | Dubious utilities for 'Vector'
module Data.MediaBus.Basics.VectorExtra
  ( castedLength,
  )
where

import Data.Vector.Storable as V

-- * Vector casting

-- | Calculate the number of elements that would fit into the given 'Vector'
-- if it were 'unsafeCast'ed to a vector of elements defined by the first argument.
castedLength ::
  forall a b proxy.
  (Storable a, Storable b) =>
  proxy a ->
  Vector b ->
  Int
castedLength _ !v = V.length (unsafeCast v :: Vector a)
