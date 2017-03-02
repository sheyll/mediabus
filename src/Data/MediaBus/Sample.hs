{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Sample
    ( SampleBuffer(..)
    , sampleBufferFromByteString
    , byteStringFromSampleBuffer
    , sampleBufferToList
    , sampleBufferFromList
    , sampleVector
    , createSampleBufferFrom
    , HasSampleBuffer(..)
    , mutateSamples
    , unsafeMutateSamples
    ) where

import           Control.Lens
import qualified Data.Vector.Storable            as SV
import           Data.Vector.Storable.Mutable    as M
import           Control.Monad.ST                ( ST, runST )
import           GHC.Exts                        ( IsList(..) )
import           Data.Typeable
import           Data.MediaBus.MediaData
import           Data.MediaBus.BlankMedia
import           Data.MediaBus.Segment
import           Data.MediaBus.Ticks
import qualified Data.ByteString                 as B
import qualified Data.Vector.Storable.ByteString as Spool
import           Data.Default
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq

-- | Something that 'IsMedia' and also has a static duration.
-- The static duration means that

newtype SampleBuffer sampleType =
      MkSampleBuffer { _sampleVector :: SV.Vector sampleType }
    deriving (Eq, Monoid, Generic)

instance NFData sampleType =>
         NFData (SampleBuffer sampleType)

instance (SV.Storable sampleType, Typeable sampleType, Show sampleType) =>
         Show (SampleBuffer sampleType) where
    show (MkSampleBuffer sampleVec) =
        let l = SV.length sampleVec
            sampleTypeRep = typeRep (Proxy :: Proxy sampleType)
            samples = SV.toList sampleVec
        in
            "SampleBuffer: " ++
                show l ++
                    " × " ++
                        show sampleTypeRep ++
                            if l > 10 then "" else " " ++ show samples

makeLenses ''SampleBuffer

instance (CanBeBlank sa, SV.Storable sa, HasDuration (Proxy sa)) =>
         CanGenerateBlankMedia (SampleBuffer sa) where
    blankFor !dur = let !sampleDuration = getDuration (Proxy :: Proxy sa)
                        !samples = ceiling (dur / sampleDuration)
                        !blankSample = blank
                    in
                        MkSampleBuffer (SV.replicate samples blankSample)

instance SV.Storable sampleType =>
         Default (SampleBuffer sampleType) where
    def = mempty

instance (HasDuration (Proxy sampleType), SV.Storable sampleType) =>
         HasDuration (SampleBuffer sampleType) where
    getDuration sb = let sampleDur = getDuration (Proxy :: Proxy sampleType)
                     in
                         sampleDur * fromIntegral (sampleCount sb)
    getDurationTicks sb = getDurationTicks (Proxy :: Proxy sampleType) *
        fromIntegral (sampleCount sb)

instance (SV.Storable a, HasDuration (Proxy a)) =>
         CanSegment (SampleBuffer a) where
    splitAfterDuration proxy buf@(MkSampleBuffer !bufV)
        | getDuration buf >= tPacket =
              let (!nextPacket, !rest) = SV.splitAt n bufV

              in
                  Just ( MkSegment (MkSampleBuffer (SV.force nextPacket))
                       , MkSampleBuffer rest
                       )
        | otherwise = Nothing
      where
        !tPacket = getStaticDuration proxy
        !n = ceiling (tPacket / tSample)
        !tSample = getDuration (Proxy :: Proxy a)

instance SV.Storable s =>
         IsList (SampleBuffer s) where
    type Item (SampleBuffer s) = s
    fromList = sampleBufferFromList
    toList = sampleBufferToList

sampleBufferFromByteString :: SV.Storable a => B.ByteString -> SampleBuffer a
sampleBufferFromByteString =
    MkSampleBuffer . Spool.byteStringToVector

byteStringFromSampleBuffer :: SV.Storable a => SampleBuffer a -> B.ByteString
byteStringFromSampleBuffer =
    Spool.vectorToByteString . _sampleVector

sampleBufferToList :: SV.Storable s => SampleBuffer s -> [s]
sampleBufferToList = SV.toList . _sampleVector

sampleBufferFromList :: SV.Storable s => [s] -> SampleBuffer s
sampleBufferFromList = MkSampleBuffer . SV.fromList

createSampleBufferFrom :: (SV.Storable sample')
                       => (forall s.
                           SV.Vector sample -> ST s (MVector s sample'))
                       -> SampleBuffer sample
                       -> SampleBuffer sample'
createSampleBufferFrom f =
    over sampleVector (\ !v -> SV.create (f v))

-- | A type class for media formats, like encodings, sample rate, etc...
class (SV.Storable (GetSampleType s), SetSampleType s (GetSampleType s) ~ s) =>
      HasSampleBuffer s where
    type SetSampleType s t
    type GetSampleType s
    sampleCount :: s -> Int
    eachSample :: SV.Storable t
               => Traversal s (SetSampleType s t) (GetSampleType s) t
    sampleBuffer :: SV.Storable t
                 => Lens s (SetSampleType s t) (SampleBuffer (GetSampleType s)) (SampleBuffer t)

instance SV.Storable a =>
         HasSampleBuffer (SampleBuffer a) where
    type GetSampleType (SampleBuffer a) = a
    type SetSampleType (SampleBuffer a) t = SampleBuffer t
    sampleCount = SV.length . _sampleVector
    eachSample = sampleBuffer . sampleVector . each
    sampleBuffer = lens id (flip const)

mutateSamples :: SV.Storable a
              => (forall s. M.MVector s a -> ST s ())
              -> SampleBuffer a
              -> SampleBuffer a
mutateSamples f (MkSampleBuffer v) =
    MkSampleBuffer (SV.modify f v)

-- | Unsafe because results can be returned, which might contain the /thawn/ vector.
unsafeMutateSamples :: SV.Storable a
                    => (forall s. M.MVector s a -> ST s r)
                    -> SampleBuffer a
                    -> (r, SampleBuffer a)
unsafeMutateSamples f (MkSampleBuffer !v) =
    runST $ do
        !mv <- SV.unsafeThaw v
        !r <- f mv
        !v' <- SV.unsafeFreeze mv
        return (r, MkSampleBuffer v')
