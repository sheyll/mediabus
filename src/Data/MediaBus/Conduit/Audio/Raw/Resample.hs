-- | Simple, preliminary (raw-)audio resampling
module Data.MediaBus.Conduit.Audio.Raw.Resample
  ( resample8to16kHz',
  )
where

import Conduit (ConduitT, MonadIO, liftIO)
import Control.Lens (mapMOf, (^.))
import Control.Monad.ST (ST)
import Control.Parallel.Strategies
  ( NFData,
    rdeepseq,
    using,
  )
import Data.MediaBus.Basics.Ticks (HasRate (..), Hz)
import Data.MediaBus.Conduit.Stream (mapFrameContentMC')
import Data.MediaBus.Media.Audio.Raw (IsPcmValue (..), Pcm)
import Data.MediaBus.Media.Buffer
  ( HasMediaBufferLens (mediaBufferLens),
    HasMediaBufferLensL,
    MediaBuffer,
    createMediaBuffer,
    mediaBufferVector,
  )
import Data.MediaBus.Media.Samples
  ( EachSample (SamplesFrom, SamplesTo),
    EachSampleL,
  )
import Data.MediaBus.Media.Stream (Stream)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.IORef
import Control.Monad (void, when)

-- | Resample 'RawAudio' 'IsMedia' from an 8kHz sample rate to 16 kHz, using a
--   simple (and fast) linear interpolation between each sample.
resample8to16kHz' ::
  forall m i s t p cIn cOut ch sa.
  ( Monad m,
    MonadIO m,
    NFData i,
    NFData s,
    NFData t,
    NFData p,
    NFData cIn,
    NFData cOut,
    HasRate cIn,
    GetRate cIn ~ Hz 8000,
    cOut ~ SetRate cIn (Hz 16000),
    HasRate cOut,
    GetRate cOut ~ Hz 16000,
    EachSampleL cIn cOut (Pcm ch sa) (Pcm ch sa),
    IsPcmValue (Pcm ch sa),
    HasMediaBufferLensL
      cIn
      cOut
      (SamplesFrom cIn)
      (SamplesTo cOut)
  ) =>
  IORef (Pcm ch sa) ->
  ConduitT (Stream i s t p cIn) (Stream i s t p cOut) m ()
resample8to16kHz' saRef = do
    mapFrameContentMC' (mapMOf mediaBufferLens resample)
  where
    resample ::
      MediaBuffer (Pcm ch sa) -> m (MediaBuffer (Pcm ch sa))
    resample !sb = do
      !lastVal <- liftIO $ atomicModifyIORef' saRef $ \ !lastVal ->
        if V.length (sb ^. mediaBufferVector) > 0
          then (V.last (sb ^. mediaBufferVector), lastVal)
          else (lastVal, lastVal)
      return (createMediaBuffer (interpolate lastVal (sb ^. mediaBufferVector)))
      where
        interpolate ::
          Pcm ch sa ->
          V.Vector (Pcm ch sa) ->
          forall st. ST st (V.MVector st (Pcm ch sa))
        interpolate !lastVal !vIn = do
          let !lenOut = 2 * lenIn
          !vOut <- M.new lenOut
          void $ lerpSamples vOut lastVal 0
          return (vOut `using` rdeepseq)
          where
            !lenIn = V.length vIn
            lerpSamples !vOut !prev !i = when (i < lenIn) $ do
              !nextHalfRate <- V.indexM vIn i
              let !next = nextHalfRate
              let !smpl = pcmAverage prev next
              M.unsafeWrite vOut (2 * i) smpl
              M.unsafeWrite vOut (2 * i + 1) next
              lerpSamples vOut next (i + 1)
