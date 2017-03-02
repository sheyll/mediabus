module Data.MediaBus.MediaData
    ( IsMedia(..)
    , MediaData(..)
    , mediaDataBuffer
    , mediaDataFromList
    , mediaDataToList
    , mediaDataFromByteString
    , mediaDataToByteString
    , createMediaData
    , modifyMediaData
    , unsafeModifyMediaData
    ) where

import           Control.Lens
import qualified Data.Vector.Storable            as V
import           Data.Vector.Storable.Mutable    as MV
import           Control.Monad.ST                ( ST, runST )
import           GHC.Exts                        ( IsList(..) )
import           Data.Typeable
import           Data.MediaBus.BlankMedia
import           Data.MediaBus.Segment
import           Data.MediaBus.Ticks
import qualified Data.ByteString                 as B
import qualified Data.Vector.Storable.ByteString as Spool
import           Data.Default
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq
import           Data.Time.Clock
import           Data.Proxy

-- | Very abstract class of media content types.
class (Eq (BufferElement t), NFData (BufferElement t), Show (Proxy t), Storable (BufferElement t)) =>
      IsMedia t where
    -- | The unit of data to be stored in 'MediaData'.
    type BufferElement t
    -- | Return the total duration of the media data.
    getMediaDataDuration :: MediaData t -> NominalDiffTime

-- | A buffer for media data. The type parameter @t@ is supposed to be an
-- instance of `IsMedia`.
newtype MediaData t = MkMediaData { _mediaDataBuffer :: V.Vector (BufferElement t)
                                  }
    deriving (Generic)

makeLenses ''MediaData

deriving instance IsMedia t => Monoid (MediaData t)

instance IsMedia t =>
         Default (MediaData t) where
    def = mempty

deriving instance IsMedia t => NFData (MediaData t)

deriving instance IsMedia t => Eq (MediaData t)

instance IsMedia s =>
         IsList (MediaData s) where
    type Item (MediaData s) = BufferElement s
    fromList = mediaDataFromList
    toList = mediaDataToList

instance IsMedia s =>
         HasDuration (MediaData s) where
    getDuration = getMediaDataDuration

instance (IsMedia a) =>
         Show (MediaData a) where
    show m = "MediaData: " ++
        show (getMediaDataDuration m) ++ " " ++ show (Proxy :: Proxy a)

-- * Conversion
-- ** List Conversion
mediaDataToList :: IsMedia s => MediaData s -> [BufferElement s]
mediaDataToList = V.toList . _mediaDataBuffer

mediaDataFromList :: IsMedia s => [BufferElement s] -> MediaData s
mediaDataFromList = MkMediaData . V.fromList

-- ** 'ByteString' Conversion
mediaDataFromByteString :: IsMedia a => B.ByteString -> MediaData a
mediaDataFromByteString =
    MkMediaData . Spool.byteStringToVector

mediaDataToByteString :: IsMedia a => MediaData a -> B.ByteString
mediaDataToByteString = Spool.vectorToByteString . _mediaDataBuffer

-- * Media data vector construction
-- | Create a new 'MediaData' using an 'ST` action that returns a mutable
-- 'MVector'.
createMediaData :: IsMedia t
                => (forall s. ST s (V.MVector s (BufferElement t)))
                -> MediaData t
createMediaData f = MkMediaData (V.create f)

-- * Fast media data manipulation
-- | Modify the underlying 'Vector' of some 'MediaData' with a function that is
-- applied to the mutable vector of that 'MediaData'. The function must result
-- in a 'ST' action that does the modifications.
modifyMediaData :: IsMedia a
                => (forall s. V.MVector s (BufferElement a) -> ST s ())
                -> MediaData a
                -> MediaData a
modifyMediaData f = over mediaDataBuffer (V.modify f)

-- | Modify the underlying 'Vector' of some 'MediaData' with a function that is
-- applied to the mutable vector of that 'MediaData'. The function must result
-- in a 'ST' action that does the modifications.
--
-- Unsafe because results can be returned, so the /thawn/ mutable vector might escape.
unsafeModifyMediaData :: IsMedia a
                      => (forall s. V.MVector s (BufferElement a) -> ST s r)
                      -> MediaData a
                      -> (r, MediaData a)
unsafeModifyMediaData f (MkMediaData !v) =
    runST $ do
        !mv <- V.unsafeThaw v
        !r <- f mv
        !v' <- V.unsafeFreeze mv
        return (r, MkMediaData v')

data Blub = B

instance IsMedia Blub where
    type BufferElement Blub = Int

xxx :: MediaData Blub
xxx = mempty
