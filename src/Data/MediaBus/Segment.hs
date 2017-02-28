{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Segment
    ( CanSegment(..)
    , Segment(..)
    , segmentContent
    ) where

import           Data.MediaBus.Ticks
import           Control.Lens
import           Data.Default
import           Data.Proxy
import           Test.QuickCheck
import           Control.DeepSeq
import           Text.Printf

-- | A segment is some content with a fixed (type level) duration.
newtype Segment (duration :: StaticTicks) c = MkSegment { _segmentContent :: c }
    deriving (NFData, Default, Arbitrary, Functor, Eq)

instance (HasStaticDuration d, Show c) =>
         Show (Segment d c) where
    show (MkSegment c) = printf "[| %s |%10s]"
                                (show c)
                                (show (getStaticDuration (Proxy :: Proxy d)))

makeLenses ''Segment

instance KnownStaticTicks d =>
         HasStaticDuration (Segment d x) where
    type SetStaticDuration (Segment d x) pt = Segment pt x
    type GetStaticDuration (Segment d x) = d

instance HasStaticDuration d =>
         HasDuration (Segment d x) where
    getDuration _ = getStaticDuration (Proxy :: Proxy d)

-- | Class of types that support splitting of from the front a packet containing
-- roughly a certain duration.
class CanSegment a where
    -- | Try to split the packet into the a part which has at most the given
    -- duration and a rest. If not possible, e.g. because the input data is
    -- already shorter than the given duration, return `Nothing`.
    splitAfterDuration :: (HasStaticDuration d)
                       => proxy d
                       -> a
                       -> Maybe (Segment d a, a)-- TODO make the repacketization create ONLY valid sized packets, even if that means dropping content
                                                       -- TODO allow repacketization to combine the packets
                                                       -- TODO use 'Segment' to automatically derive the ptime
