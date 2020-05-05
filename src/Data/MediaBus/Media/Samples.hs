-- | Media represented by samples in the time domain.
--   What is meant by media sample?
--   For example the microphone value captured at a certain point in time for mono audio,
--   or the pair of values from the left and right channel of a stereo audio track,
--   or a pixel; the common characteristic is that a sample encompasses
--   the (aggregate) value representing smallest piece of media.
--   A sample may be composed of components, like in a stereo audio sample,
--   or color channels in an YUV pixel, but since they are played/recorded at
--   the same time instances, they form a sample only when combined not when
--   regarded indivudually.
module Data.MediaBus.Media.Samples
  ( IsSampledMedia (),
    type CanBeSample,
    EachSample (..),
    type EachSampleL,
    type EachSampleL',
    type EachSample',
    eachSample',
  )
where

import Control.DeepSeq
import Control.Lens
import Data.MediaBus.Basics.Ticks
import Data.MediaBus.Media.Media
import Data.Typeable
import Foreign.Storable

-- | Types that are 'IsMedia' instances with sampled content.
class
  (IsMedia i, HasRate i) =>
  IsSampledMedia i

-- | Always recurring contraints on 'Sample' types.
type CanBeSample i = (Typeable i, Eq i, NFData i, Storable i)

-- | A type class for types that contain samples which can be converted into
-- other sample types.
class
  (CanBeSample (SamplesFrom s), CanBeSample (SamplesTo t)) =>
  EachSample s t where
  -- | The type of the contained media samples passed as input by 'eachSample'.
  type SamplesFrom s

  -- | The type of the contained media samples from the output of 'eachSample'.
  type SamplesTo t

  --  | Traversal for accessing each individual sample.
  -- The default implementation uses an 'EachSampleL' instance
  eachSample :: Traversal s t (SamplesFrom s) (SamplesTo t)

-- | A lens-stype type alias for 'EachSample' with the full @s t a b@ parameter set.
type EachSampleL s t a b = (EachSample s t, SamplesFrom s ~ a, SamplesTo t ~ b)

-- | A lens-stype type alias for 'EachSample' for cases where
--  @'SamplesFrom' ~ 'SamplesTo'@ with the full @s a@ parameter set.
type EachSampleL' s a = (EachSample s s, SamplesFrom s ~ a, SamplesTo s ~ a)

-- | A lens-stype type alias for 'EachSample' for cases where @'SamplesFrom' ~ 'SamplesTo'@
type EachSample' s = (EachSample s s, SamplesFrom s ~ SamplesTo s)

-- | A variant of 'eachSample' for cases where @'SamplesFrom' ~ 'SamplesTo'@.
eachSample' ::
  EachSample' i =>
  Traversal' i (SamplesFrom i)
eachSample' = eachSample
