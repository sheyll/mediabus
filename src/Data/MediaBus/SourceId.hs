module Data.MediaBus.SourceId
    ( HasSourceIdT(..)
    , SourceId(..)
    , type SrcId32
    , type SrcId64
    , sourceId
    ) where

import           Control.Lens
import           Test.QuickCheck
import           Data.Default
import           GHC.Generics    ( Generic )
import           Control.DeepSeq
import           Data.Word

class SetSourceId a (GetSourceId a) ~ a =>
      HasSourceIdT a where
    type GetSourceId a
    type SetSourceId a b

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
newtype SourceId i = MkSourceId { _sourceId :: i }
    deriving (Eq, Arbitrary, Default, Ord, Generic)

type SrcId32 = SourceId Word32

type SrcId64 = SourceId Word64

instance (NFData i) =>
         NFData (SourceId i)

makeLenses ''SourceId

instance Show i =>
         Show (SourceId i) where
    show (MkSourceId x) = "SOURCE-ID: " ++ show x
