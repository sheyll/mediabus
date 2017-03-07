-- | Conduit combinators that make the type of a conduit unambiguous to the
-- compiler via proxies.
module Data.MediaBus.Conduit.TypeAnnotations
  ( annotateTypeC
  , annotateTypeCIn
  , annotateTypeCOut
  , annotateTypeSource
  , annotateTypeSink
  ) where

import Conduit

-- * Generic Conduits
-- | Annotate the type of a 'Conduit' where input and output are the same
annotateTypeC :: proxy a -> Conduit a m a -> Conduit a m a
annotateTypeC _ = id

-- | Annotate the input type of a 'Conduit'
annotateTypeCIn :: proxy a -> Conduit a m b -> Conduit a m b
annotateTypeCIn _ = id

-- | Annotate the output type of a 'Conduit'
annotateTypeCOut :: proxy b -> Conduit a m b -> Conduit a m b
annotateTypeCOut _ = id

-- | Annotate the output type of a 'Source'
annotateTypeSource :: proxy a -> Source m a -> Source m a
annotateTypeSource _ = id

-- | Annotate the input type of a 'Sink'
annotateTypeSink :: proxy a -> Sink a m r -> Sink a m r
annotateTypeSink _ = id
