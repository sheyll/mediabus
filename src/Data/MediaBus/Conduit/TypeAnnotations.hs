-- | Conduit combinators that make the type of a conduit unambiguous to the
-- compiler via proxies.
module Data.MediaBus.Conduit.TypeAnnotations
  ( annotateTypeC,
    annotateTypeCIn,
    annotateTypeCOut,
    annotateTypeSource,
    annotateTypeSink,
  )
where

import Conduit

-- * Generic Conduits

-- | Annotate the type of a 'Conduit' where input and output are the same
annotateTypeC :: proxy a -> ConduitT a a m () -> ConduitT a a m ()
annotateTypeC _ = id

-- | Annotate the input type of a 'Conduit'
annotateTypeCIn :: proxy a -> ConduitT a b m () -> ConduitT a b m ()
annotateTypeCIn _ = id

-- | Annotate the output type of a 'Conduit'
annotateTypeCOut :: proxy b -> ConduitT a b m () -> ConduitT a b m ()
annotateTypeCOut _ = id

-- | Annotate the output type of a 'Source'
annotateTypeSource :: proxy a -> ConduitT () a m () -> ConduitT () a m ()
annotateTypeSource _ = id

-- | Annotate the input type of a 'Sink'
annotateTypeSink :: proxy a -> ConduitT a Void m r -> ConduitT a Void m r
annotateTypeSink _ = id
