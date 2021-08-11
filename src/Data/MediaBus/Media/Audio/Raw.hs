{-# LANGUAGE UndecidableInstances #-}

-- | Sub-types of 'Audio' for uncompressed audio.
module Data.MediaBus.Media.Audio.Raw
  ( Raw,
    Pcm,
    pcmMediaBuffer,
    IsPcmValue (..),
  )
where

import Control.DeepSeq (NFData (..))
import Control.Lens (Each (each), Iso, iso, (#), (^.))
import Data.MediaBus.Basics.Ticks
  ( HasDuration (getDuration),
    KnownRate (rateVal),
    RateProxy (MkRateProxy),
    getPeriodDuration,
    getStaticDuration,
  )
import Data.MediaBus.Media.Audio (Audio)
import Data.MediaBus.Media.Blank
  ( CanBeBlank (..),
    CanGenerateBlankMedia (blankFor),
  )
import Data.MediaBus.Media.Buffer
  ( CanBeSample,
    HasMediaBuffer (..),
    MediaBuffer (MkMediaBuffer),
    createMediaBuffer,
    mediaBufferLength,
    mediaBufferVector,
  )
import Data.MediaBus.Media.Channels
  ( EachChannel (..),
    EachChannelL,
    KnownChannelLayout,
  )
import Data.MediaBus.Media.Media (IsMedia, MediaDescription (..))
import Data.MediaBus.Media.Samples (EachSample (..), EachSampleL)
import Data.MediaBus.Media.Segment
  ( CanSegment (..),
    segmentContent,
  )
import Data.Typeable (Proxy (..), Typeable, typeRep)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Test.QuickCheck (Arbitrary)

-- | An indicator for uncompressed audio with a given per sample encoding type.
data Raw encoding

-- | A family of multi-channel audio sample value types. Values of this type are
--   stored in 'MediaBuffer's.
--   The 'Audio' instances with 'Raw' encodings use 'MediaBuffer's of 'Pcm'
--   values to store (multi-channel-) samples.
data family Pcm c t

-- | Types of per channel PCM audio sample value.
class
  (CanBeBlank a, CanBeSample a, Arbitrary a) =>
  IsPcmValue a
  where
  -- | Calculate the average of two pcm samples
  pcmAverage :: a -> a -> a

-- | All 'Pcm' audio is audio. An 'Audio' instance with 'Pcm's in a 'MediaBuffer'.
newtype instance Audio r c (Raw t) = MkPcm {_pcmMediaBuffer :: MediaBuffer (Pcm c t)}

-- | An isomorphism for 'Audio' and 'MediaBuffer'
pcmMediaBuffer :: Iso (Audio r c (Raw t)) (Audio r' c' (Raw t')) (MediaBuffer (Pcm c t)) (MediaBuffer (Pcm c' t'))
pcmMediaBuffer = iso _pcmMediaBuffer MkPcm

instance
  (KnownRate r, CanBeSample (Pcm c t), CanBeBlank (Pcm c t)) =>
  CanGenerateBlankMedia (Audio r c (Raw t))
  where
  blankFor dt =
    MkPcm $
      createMediaBuffer $
        let numberOfSamples =
              ceiling (dt * fromIntegral (rateVal (MkRateProxy :: RateProxy r)))
         in M.replicate numberOfSamples blank

instance
  (KnownRate r, CanBeSample (Pcm c t)) =>
  HasDuration (Audio r c (Raw t))
  where
  getDuration (MkPcm !buf) =
    let pd = getPeriodDuration (Proxy :: Proxy (Audio r c (Raw t)))
        n = fromIntegral $ mediaBufferLength buf
     in pd * n

instance
  CanBeSample (Pcm c t) =>
  NFData (Audio r c (Raw t))
  where
  rnf (MkPcm !buf) = rnf buf

instance
  CanBeSample (Pcm c t) =>
  Eq (Audio r c (Raw t))
  where
  (==) (MkPcm !buf1) (MkPcm !buf2) = buf1 == buf2

instance
  (Typeable t, KnownRate r, KnownChannelLayout c, CanBeSample (Pcm c t)) =>
  IsMedia (Audio r c (Raw t))

instance
  (Typeable t, KnownRate r, KnownChannelLayout c) =>
  Show (MediaDescription (Audio r c (Raw t)))
  where
  showsPrec d _ =
    showParen
      (d > 10)
      ( showString "pcm-audio "
          . showsPrec 11 (typeRep (Proxy :: Proxy t))
          . showString " "
          . showsPrec 11 (MkRateProxy :: RateProxy r)
          . showString " "
          . showsPrec 11 (MkRateProxy :: RateProxy r)
      )

instance
  (Typeable t, KnownRate r, KnownChannelLayout c, CanBeSample (Pcm c t), Show (Pcm c t)) =>
  Show (Audio r c (Raw t))
  where
  showsPrec d (MkPcm !c) =
    showParen (d > 10) $
      shows (MkShowMedia :: MediaDescription (Audio r c (Raw t))) . showsPrec 11 c

instance
  (CanBeSample (Pcm ca a), CanBeSample (Pcm cb b)) =>
  HasMediaBuffer (Audio r ca (Raw a)) (Audio r' cb (Raw b))
  where
  type MediaBufferFrom (Audio r ca (Raw a)) = (MediaBuffer (Pcm ca a))
  type MediaBufferTo (Audio r' cb (Raw b)) = (MediaBuffer (Pcm cb b))
  mediaBuffer = pcmMediaBuffer

instance
  (CanBeSample (Pcm c t), CanBeSample (Pcm c' t')) =>
  EachSample (Audio r c (Raw t)) (Audio r' c' (Raw t'))
  where
  type SamplesFrom (Audio r c (Raw t)) = (Pcm c t)
  type SamplesTo (Audio r' c' (Raw t')) = (Pcm c' t')
  eachSample = pcmMediaBuffer . each

instance
  ( CanBeSample (Pcm c t),
    CanBeSample (Pcm c' t'),
    EachSampleL (Audio r c (Raw t)) (Audio r' c' (Raw t')) (Pcm c t) (Pcm c' t'),
    EachChannelL (Pcm c t) (Pcm c' t') t t'
  ) =>
  EachChannel (Audio r c (Raw t)) (Audio r' c' (Raw t'))
  where
  type ChannelsFrom (Audio r c (Raw t)) = ChannelsFrom (Pcm c t)
  type ChannelsTo (Audio r' c' (Raw t')) = ChannelsTo (Pcm c' t')
  eachChannel = eachSample . eachChannel

instance
  (CanBeSample (Pcm c t), KnownRate r) =>
  CanSegment (Audio r c (Raw t))
  where
  splitAfterDuration proxy buf@(MkPcm (MkMediaBuffer !bufV))
    | getDuration buf >= tPacket =
      let (!nextPacket, !rest) = V.splitAt n bufV
       in Just
            ( segmentContent . pcmMediaBuffer . mediaBufferVector # V.force nextPacket,
              pcmMediaBuffer . mediaBufferVector # rest
            )
    | otherwise = Nothing
    where
      !tPacket = getStaticDuration proxy
      !n = ceiling (tPacket / tSample)
      !tSample = getPeriodDuration (Proxy :: Proxy (Audio r c (Raw t)))

instance CanBeSample (Pcm c t) => Semigroup (Audio r c (Raw t)) where
  x <> y = pcmMediaBuffer # mappend (x ^. pcmMediaBuffer) (y ^. pcmMediaBuffer)

instance CanBeSample (Pcm c t) => Monoid (Audio r c (Raw t)) where
  mempty = pcmMediaBuffer # mempty
