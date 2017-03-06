module Data.MediaBus.Payload ( HasPayload(..), type HasPayload', payload' ) where

import           Control.Lens

class HasPayload s t a b | s -> a, t -> b where
    payload :: Traversal s t a b

type HasPayload' s a = HasPayload s s a a

payload' :: HasPayload' s a => Traversal' s a
payload' = payload
