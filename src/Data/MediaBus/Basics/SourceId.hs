-- | This module contains the 'SourceId' wrapper type, that indicate that
-- something is identifies a single source of media. This could be an RTP SSRC
-- or the IP/Port pair of a network source. The defining characteristic of a
-- 'SourceId' is that every thing that has a certain source id stems from **the
-- same media source**, e.g. the same microphone, audio file, synthesizer,...
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


-- | An 'Iso' for the value of a 'SourceId'
sourceIdValue :: Iso (SourceId a) (SourceId b) a b
sourceIdValue = iso _sourceIdValue MkSourceId

-- | A short alias for 'SourceId' with a 'Word32' value
type SrcId32 = SourceId Word32

-- | A short alias for 'SourceId' with a 'Word64' value
type SrcId64 = SourceId Word64

instance (NFData i) =>
         NFData (SourceId i)

instance Show i =>
         Show (SourceId i) where
    show (MkSourceId x) = "SOURCE-ID: " ++ show x

-- | Type class for a lens over the contained source id
class HasSourceId s t where
    type SourceIdFrom s
    type SourceIdTo t
    -- | A lens for the 'SourceId'
    sourceId :: Lens s t (SourceIdFrom s) (SourceIdTo t)

-- | Type class with a 'Traversal' for types that may or may not contain anctual
-- source id.
class EachSourceId s t where
    type SourceIdsFrom s
    type SourceIdsTo t
    -- | A 'Traversal' for the 'SourceId'
    eachSourceId :: Traversal s t (SourceIdsFrom s) (SourceIdsTo t)
