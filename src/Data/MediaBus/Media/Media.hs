-- | This module contains mainly the informative 'IsMedia' type class and
-- corresponding type classes for media lenses.
module Data.MediaBus.Media.Media
  ( IsMedia()
  , MediaDescription(..)
  , HasMedia(..)
  , type HasMedia'
  , media'
  , type HasMediaL
  , type HasMediaL'
  , EachMedia(..)
  , type EachMedia'
  , eachMedia'
  , type EachMediaL
  , type EachMediaL'
  ) where

import Control.DeepSeq
import Control.Lens

-- * Media
-- | Very abstract class of media content types. The only universal feature is that it has a static description.
class ( NFData i
      , Show (MediaDescription i) -- TODO HasDuration instance??
      ) =>
      IsMedia i

-- | A proxy type to display static media info via a 'Show' instance
data MediaDescription i where
        MkShowMedia :: (Show (MediaDescription i)) => MediaDescription i

-- * Media container 'Lens'es
-- | Types that contain an 'IsMedia' instance.
class (IsMedia (MediaFrom s), IsMedia (MediaTo t)) =>
      HasMedia s t where
  type MediaFrom s
  type MediaTo t
  -- | A 'Lens' for the media within 's'
  media :: Lens s t (MediaFrom s) (MediaTo t)

-- | Types that contain an 'IsMedia' instance.
type HasMedia' s = (IsMedia (MediaFrom s), HasMedia s s, MediaFrom s ~ MediaTo s)

-- | A simple 'Lens' for the media within 's'
media'
  :: HasMedia' s
  => Lens' s (MediaFrom s)
media' = media

-- | Types that contain an 'IsMedia' instance with the traditional lens-like type parameters
type HasMediaL s t a b = (IsMedia a, HasMedia s t, MediaFrom s ~ a, MediaTo t ~ b)

-- | Like 'HasMedia' but with the **simple** lens-like type parameters
type HasMediaL' s a = (IsMedia a, HasMedia s s, MediaFrom s ~ a, MediaTo s ~ a, MediaFrom s ~ MediaTo s)

-- * Media container 'Traversal's
-- | Types that contain zero or more 'IsMedia' instance.
class (IsMedia (MediaFromE s), IsMedia (MediaToE t)) =>
      EachMedia s t where
  -- | The contained input media type, since the name 'MediaFrom' was taken
  -- , this is named 'MediaFromE' where the @E@ is supposed to hint at the fact
  -- that this class is called 'EachMedia'.
  type MediaFromE s
  type MediaToE t
  -- | A 'Traversal' for the media within 's'
  eachMedia :: Traversal s t (MediaFromE s) (MediaToE t)
  default eachMedia :: HasMediaL s t (MediaFromE s) (MediaToE t) =>
    Traversal s t (MediaFromE s) (MediaToE t)
  eachMedia = media

-- | Types that contain zero or more 'IsMedia' instance.
type EachMedia' s = (IsMedia (MediaFromE s), EachMedia s s, MediaFromE s ~ MediaToE s)

-- | A simple 'Traversal' for the media within 's'
eachMedia'
  :: EachMedia' s
  => Traversal' s (MediaFromE s)
eachMedia' = eachMedia

-- | Types that contain zero or more 'IsMedia' instance with the traditional lens-like type parameters
type EachMediaL s t a b = (IsMedia a, EachMedia s t, MediaFromE s ~ a, MediaToE t ~ b)

-- | Like 'EachMedia' but with the **simple** lens-like type parameters
type EachMediaL' s a = (IsMedia a, EachMedia s s, MediaFromE s ~ a, MediaToE s ~ a, MediaFromE s ~ MediaToE s)
