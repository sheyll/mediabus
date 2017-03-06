module Data.MediaBus.Basics.SourceId
    ( SourceId(..)
    , sourceIdValue
    , type SrcId32
    , type SrcId64
    , HasSourceId(..)
    , EachSourceId(..)
    ) where

import           Control.Lens
import           Test.QuickCheck
import           Data.Default
import           GHC.Generics    ( Generic )
import           Control.DeepSeq
import           Data.Word

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
newtype SourceId i = MkSourceId { _sourceIdValue :: i }
    deriving (Eq, Arbitrary, Default, Ord, Generic)

type SrcId32 = SourceId Word32

type SrcId64 = SourceId Word64

instance (NFData i) =>
         NFData (SourceId i)

makeLenses ''SourceId

instance Show i =>
         Show (SourceId i) where
    show (MkSourceId x) = "SOURCE-ID: " ++ show x

class HasSourceId s t where
    type SourceIdFrom s
    type SourceIdTo t
    sourceId :: Lens s t (SourceIdFrom s) (SourceIdTo t)

class EachSourceId s t where
    type SourceIdsFrom s
    type SourceIdsTo t
    eachSourceId :: Traversal s t (SourceIdsFrom s) (SourceIdsTo t)
