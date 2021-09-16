-- | Media with a content stored in a 'Vector'
module Data.MediaBus.Media.Buffer
  ( type CanBeSample,
    HasMediaBufferLens (..),
    type HasMediaBufferLens',
    type HasMediaBufferLensL,
    type HasMediaBufferLensL',
    mediaBufferLens',
    MediaBuffer (..),
    mediaBufferVector,
    mediaBufferLength,
    mediaBufferFromList,
    mediaBufferToList,
    mediaBufferFromByteString,
    mediaBufferToByteString,
    createMediaBuffer,
    modifyMediaBuffer,
    unsafeModifyMediaBuffer,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens
  ( Each (..),
    Index,
    Iso,
    IxValue,
    Ixed (ix),
    Lens,
    Lens',
    iso,
    over,
    to,
    view,
  )
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import Data.Default (Default (..))
import Data.MediaBus.Media.Samples (CanBeSample)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.ByteString as Spool
import Foreign.Storable
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Test.QuickCheck

-- ** Types using 'MediaBuffer'

-- | Types providing a 'MediaBuffer' lens
class (Storable (MediaBufferElemFrom s), Storable (MediaBufferElemTo s))
  => HasMediaBufferLens s t where
  type MediaBufferElemFrom s
  type MediaBufferElemTo t
  mediaBufferLens :: Lens s t (MediaBuffer (MediaBufferElemFrom s)) (MediaBuffer (MediaBufferElemTo t))

-- | Like 'HasMediaBufferLens' but with @s ~ t@ and @MediaBufferElemFrom s ~ MediaBufferElemTo t@
type HasMediaBufferLens' s = (HasMediaBufferLens s s, MediaBufferElemFrom s ~ MediaBufferElemTo s)

-- | Like 'mediaBuffer' but with @s ~ t@ and @MediaBufferFrom s ~ MediaBufferTo t@
mediaBufferLens' ::
  HasMediaBufferLens' s =>
  Lens' s (MediaBuffer (MediaBufferElemTo s))
mediaBufferLens' = mediaBufferLens

-- | Like 'HasMediaBuffer' but with the typical lens type parameters @s t a b@
type HasMediaBufferLensL s t a b = (HasMediaBufferLens s t, MediaBufferElemFrom s ~ a, MediaBufferElemTo t ~ b)

-- | Like 'HasMediaBufferL' but with the typical **simple** lens type parameters @s a@
type HasMediaBufferLensL' s a = (HasMediaBufferLens s s, MediaBufferElemFrom s ~ a, MediaBufferElemTo s ~ a)

-- | A buffer for media data. This is just a newtype wrapper around 'V.Vector'.
newtype MediaBuffer t = MkMediaBuffer
  { _mediaBufferVector :: V.Vector t
  }
  deriving (Generic, NFData, Semigroup, Monoid, Eq)

-- | 'MediaBuffer' to 'Vector' isomorphism
mediaBufferVector :: Iso (MediaBuffer s) (MediaBuffer t) (V.Vector s) (V.Vector t)
mediaBufferVector = iso _mediaBufferVector MkMediaBuffer

-- ** Instances

instance (Storable t, Arbitrary t) => Arbitrary (MediaBuffer t) where
  arbitrary = mediaBufferFromList <$> listOf arbitrary

instance
  (V.Storable a, V.Storable b) =>
  Each (MediaBuffer a) (MediaBuffer b) a b
  where
  each = mediaBufferVector . each

type instance IxValue (MediaBuffer a) = a

type instance Index (MediaBuffer a) = Int

instance Storable a => Ixed (MediaBuffer a) where
  ix i = mediaBufferVector . ix i

instance
  Storable s =>
  IsList (MediaBuffer s)
  where
  type Item (MediaBuffer s) = s
  fromList = mediaBufferFromList
  toList = mediaBufferToList

instance
  (Show a, Typeable a, Storable a) =>
  Show (MediaBuffer a)
  where
  showsPrec d m =
    showParen (d > 10) $
      showString "media buffer: "
        . showsPrec 8 (mediaBufferLength m)
        . showString " * "
        . showsPrec 8 (typeRep (Proxy :: Proxy a))

instance
  (Storable sampleType, Monoid sampleType) =>
  Default (MediaBuffer sampleType)
  where
  def = mempty

-- | Return the number of 'BufferElement's in the 'MediaBuffer' buffer.
mediaBufferLength ::
  Storable t =>
  MediaBuffer t ->
  Int
mediaBufferLength = view (mediaBufferVector . to V.length)

-- ** Conversion

-- *** List Conversion

-- | Convert the media buffer vector contents to a list in O(n).
mediaBufferToList ::
  Storable s =>
  MediaBuffer s ->
  [s]
mediaBufferToList = view (mediaBufferVector . to V.toList)

-- | Create a 'MediaBuffer' from a list in O(n).
mediaBufferFromList ::
  Storable s =>
  [s] ->
  MediaBuffer s
mediaBufferFromList = MkMediaBuffer . V.fromList

-- *** 'ByteString' Conversion

-- | An efficient conversion of a 'ByteString' to a 'MediaBuffer'
mediaBufferFromByteString ::
  Storable a =>
  B.ByteString ->
  MediaBuffer a
mediaBufferFromByteString = MkMediaBuffer . Spool.byteStringToVector

-- | An efficient conversion of a 'MediaBuffer' to a 'ByteString'
mediaBufferToByteString ::
  Storable a =>
  MediaBuffer a ->
  B.ByteString
mediaBufferToByteString = Spool.vectorToByteString . _mediaBufferVector

-- ** Construction

-- | Create a new 'MediaBuffer' using an 'ST` action that returns a mutable
-- 'MVector'.
createMediaBuffer ::
  Storable t =>
  (forall s. ST s (V.MVector s t)) ->
  MediaBuffer t
createMediaBuffer f = MkMediaBuffer (V.create f)

-- ** Fast manipulation

-- | Modify the underlying 'Vector' of some 'MediaBuffer' with a function that is
-- applied to the mutable vector of that 'MediaBuffer'. The function must result
-- in a 'ST' action that does the modifications.
modifyMediaBuffer ::
  Storable a =>
  (forall s. V.MVector s a -> ST s ()) ->
  MediaBuffer a ->
  MediaBuffer a
modifyMediaBuffer f = over mediaBufferVector (V.modify f)

-- | Modify the underlying 'Vector' of some 'MediaBuffer' with a function that is
-- applied to the mutable vector of that 'MediaBuffer'. The function must result
-- in a 'ST' action that does the modifications.
--
-- Unsafe because results can be returned, so the /thawn/ mutable vector might escape.
unsafeModifyMediaBuffer ::
  Storable a =>
  (forall s. V.MVector s a -> ST s r) ->
  MediaBuffer a ->
  (r, MediaBuffer a)
unsafeModifyMediaBuffer f (MkMediaBuffer !v) =
  runST $ do
    !mv <- V.unsafeThaw v
    !r <- f mv
    !v' <- V.unsafeFreeze mv
    return (r, MkMediaBuffer v')
