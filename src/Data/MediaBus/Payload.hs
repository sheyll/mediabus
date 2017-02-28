module Data.MediaBus.Payload ( HasPayload(..) ) where

import           Control.Lens

class (SetPayload a (GetPayload a) ~ a) =>
      HasPayload a where
    type GetPayload a
    type SetPayload a b
    payload :: Traversal a (SetPayload a b) (GetPayload a) b
