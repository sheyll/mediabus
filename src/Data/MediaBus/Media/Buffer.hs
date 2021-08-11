-- | Media with a content stored in a 'Vector'
module Data.MediaBus.Media.Buffer
  ( type CanBeSample,
    HasMediaBuffer (..),
    type HasMediaBuffer',
    type HasMediaBufferL,
    type HasMediaBufferL',
    mediaBuffer',
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
    Iso,
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
import Data.Typeable (Proxy (..), typeRep)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.ByteString as Spool
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

-- ** Types using 'MediaBuffer'

-- TODO simplify, get rid of useless type classes!!!

-- | Types containing a 'MediaBuffer'
class HasMediaBuffer s t where
  -- | The media buffer type contained in 's'
  type MediaBufferFrom s

  -- | The type that results from changing the media buffer type in 's' to 'b'
  type MediaBufferTo t

  -- | A lens for converting the media buffer
  mediaBuffer :: Lens s t (MediaBufferFrom s) (MediaBufferTo t)

-- | Like 'HasMediaBuffer' but with @s ~ t@ and @MediaBufferFrom s ~ MediaBufferTo t@
type HasMediaBuffer' s = (HasMediaBuffer s s, MediaBufferFrom s ~ MediaBufferTo s)

-- | Like 'mediaBuffer' but with @s ~ t@ and @MediaBufferFrom s ~ MediaBufferTo t@
mediaBuffer' ::
  HasMediaBuffer' s =>
  Lens' s (MediaBufferFrom s)
mediaBuffer' = mediaBuffer

-- | Like 'HasMediaBuffer' but with the typical lens type parameters @s t a b@
type HasMediaBufferL s t a b = (HasMediaBuffer s t, MediaBufferFrom s ~ a, MediaBufferTo t ~ b)

-- | Like 'HasMediaBufferL' but with the typical **simple** lens type parameters @s a@
type HasMediaBufferL' s a = (HasMediaBuffer s s, MediaBufferFrom s ~ a, MediaBufferTo s ~ a)

-- | A buffer for media data. This is just a newtype wrapper around 'V.Vector'.
newtype MediaBuffer t = MkMediaBuffer
  { _mediaBufferVector :: V.Vector t
  }
  deriving (Generic, NFData, Semigroup, Monoid, Eq)

-- | 'MediaBuffer' to 'Vector' isomorphism
mediaBufferVector :: Iso (MediaBuffer s) (MediaBuffer t) (V.Vector s) (V.Vector t)
mediaBufferVector = iso _mediaBufferVector MkMediaBuffer

-- | Traversal instance
instance
  (V.Storable a, V.Storable b) =>
  Each (MediaBuffer a) (MediaBuffer b) a b
  where
  each = mediaBufferVector . each

-- ** Instances

instance
  CanBeSample s =>
  IsList (MediaBuffer s)
  where
  type Item (MediaBuffer s) = s
  fromList = mediaBufferFromList
  toList = mediaBufferToList

instance
  (Show a, CanBeSample a) =>
  Show (MediaBuffer a)
  where
  showsPrec d m =
    showParen (d > 10) $
      showString "media buffer: "
        . showsPrec 8 (mediaBufferLength m)
        . showString " * "
        . showsPrec 8 (typeRep (Proxy :: Proxy a))

instance
  CanBeSample sampleType =>
  Default (MediaBuffer sampleType)
  where
  def = mempty

-- | Return the number of 'BufferElement's in the 'MediaBuffer' buffer.
mediaBufferLength ::
  CanBeSample t =>
  MediaBuffer t ->
  Int
mediaBufferLength = view (mediaBufferVector . to V.length)

-- ** Conversion

-- *** List Conversion

-- | Convert the media buffer vector contents to a list in O(n).
mediaBufferToList ::
  CanBeSample s =>
  MediaBuffer s ->
  [s]
mediaBufferToList = view (mediaBufferVector . to V.toList)

-- | Create a 'MediaBuffer' from a list in O(n).
mediaBufferFromList ::
  CanBeSample s =>
  [s] ->
  MediaBuffer s
mediaBufferFromList = MkMediaBuffer . V.fromList

-- *** 'ByteString' Conversion

-- | An efficient conversion of a 'ByteString' to a 'MediaBuffer'
mediaBufferFromByteString ::
  CanBeSample a =>
  B.ByteString ->
  MediaBuffer a
mediaBufferFromByteString = MkMediaBuffer . Spool.byteStringToVector

-- | An efficient conversion of a 'MediaBuffer' to a 'ByteString'
mediaBufferToByteString ::
  CanBeSample a =>
  MediaBuffer a ->
  B.ByteString
mediaBufferToByteString = Spool.vectorToByteString . _mediaBufferVector

-- ** Construction

-- | Create a new 'MediaBuffer' using an 'ST` action that returns a mutable
-- 'MVector'.
createMediaBuffer ::
  CanBeSample t =>
  (forall s. ST s (V.MVector s t)) ->
  MediaBuffer t
createMediaBuffer f = MkMediaBuffer (V.create f)

-- ** Fast manipulation

-- | Modify the underlying 'Vector' of some 'MediaBuffer' with a function that is
-- applied to the mutable vector of that 'MediaBuffer'. The function must result
-- in a 'ST' action that does the modifications.
modifyMediaBuffer ::
  CanBeSample a =>
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
  CanBeSample a =>
  (forall s. V.MVector s a -> ST s r) ->
  MediaBuffer a ->
  (r, MediaBuffer a)
unsafeModifyMediaBuffer f (MkMediaBuffer !v) =
  runST $ do
    !mv <- V.unsafeThaw v
    !r <- f mv
    !v' <- V.unsafeFreeze mv
    return (r, MkMediaBuffer v')
