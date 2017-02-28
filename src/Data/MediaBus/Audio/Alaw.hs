module Data.MediaBus.Audio.Alaw
    ( ALaw(..)
    , alawSample
    ) where

import           Foreign.Storable
import           Data.MediaBus.Stream
import           Data.MediaBus.Audio.Raw
import           Data.MediaBus.Audio.Channels
import           Data.MediaBus.BlankMedia
import           Data.MediaBus.Ticks
import           Data.MediaBus.Sample
import           Data.MediaBus.Transcoder
import           Data.Bits
import           Data.Word
import           Data.Int
import           Control.Lens
import           Data.Proxy
import           Data.Function                ( on )
import           Test.QuickCheck              ( Arbitrary(..) )
import           GHC.Generics                 ( Generic )
import           Control.Parallel.Strategies  ( NFData, rdeepseq, withStrategy )

newtype ALaw = MkALaw { _alawSample :: Word8 }
    deriving (Show, Storable, Num, Eq, Bits, Arbitrary, Generic)

instance NFData ALaw

makeLenses ''ALaw

instance Ord ALaw where
    compare = compare `on` (decodeALawSample . _alawSample)

instance HasDuration (Proxy ALaw) where
    getDuration _ = 1 / 8000
    getDurationTicks _ = convertTicks (MkTicks 1 :: Ticks 8000 Int)

instance HasChannelLayout ALaw where
    channelLayout _ = SingleChannel

instance Transcoder (SampleBuffer ALaw) (SampleBuffer (S16 8000)) where
    transcode = over (framePayload . eachSample)
                     (withStrategy rdeepseq .
                          MkS16 . decodeALawSample . _alawSample)

instance Transcoder (SampleBuffer (S16 8000)) (SampleBuffer ALaw) where
    transcode = over (framePayload . eachSample)
                     (withStrategy rdeepseq .
                          MkALaw . encodeALawSample . _s16Sample)

instance IsAudioSample ALaw where
    type GetAudioSampleRate ALaw = 8000
    type SetAudioSampleRate ALaw x = ALaw
    avgSamples !x !y = MkALaw .
        encodeALawSample .
            _s16Sample $
        (avgSamples `on` (mkS16 . decodeALawSample . _alawSample)) x y
      where
        mkS16 :: Int16 -> S16 8000
        mkS16 = MkS16
    setAudioSampleRate _ = id

instance CanBeBlank ALaw where
    blank = MkALaw 0xD5

decodeALawSample :: Word8 -> Int16
decodeALawSample !a' = let !a = a' `xor` 85
                           !quant_mask = 15
                           !quant_shift = 4
                           !seg_mask = 112
                           !seg_shift = 4
                           tBase, tAbs, seg :: Int16
                           !seg = (fromIntegral a .&. seg_mask) `shiftR`
                               seg_shift
                           !tBase = (fromIntegral a .&. quant_mask) `shiftL`
                               quant_shift
                           !tAbs = case seg of
                               0 -> tBase + 8
                               1 -> tBase + 264
                               _ -> (tBase + 264) `shiftL`
                                   fromIntegral (seg - 1)
                           !isPos = testBit a 7
                       in
                           if isPos then tAbs else tAbs * (-1)

-- | See http://opensource.apple.com//source/tcl/tcl-20/tcl_ext/snack/snack/generic/g711.c
--
-- >	Linear Input Code	 Compressed Code
-- >	-----------------  ---------------
-- >	  0000000wxyza         000wxyz
-- >	  0000001wxyza         001wxyz
-- >	  000001wxyzab         010wxyz
-- >	  00001wxyzabc         011wxyz
-- >	  0001wxyzabcd         100wxyz
-- >	  001wxyzabcde         101wxyz
-- >	  01wxyzabcdef         110wxyz
-- >	  1wxyzabcdefg         111wxyz
--
-- For further information see John C. Bellamy's Digital Telephony, 1982, John
-- Wiley & Sons, pps 98-111 and 472-476.
encodeALawSample :: Int16 -> Word8
encodeALawSample !pcmVal' =
    let !pcmVal = pcmVal' `shiftR` 3 -- to 13 bit
        (!mask, !pcmValAbs) = if pcmVal >= 0
                              then ( 0xD5 -- sign (7th) bit = 1
                                   , pcmVal
                                   )
                              else ( 0x55 -- sign bit = 0
                                   , (-1) * pcmVal - 1
                                   )
        -- !segments = [0x1F,0x3F,0x7F,0xFF,0x1FF,0x3FF,0x7FF,0xFFF] :: [
        !segment
            | pcmValAbs <= 31 = 0
            | pcmValAbs <= 63 = 1
            | pcmValAbs <= 127 = 2
            | pcmValAbs <= 255 = 3
            | pcmValAbs <= 511 = 4
            | pcmValAbs <= 1023 = 5
            | pcmValAbs <= 2047 = 6
            | pcmValAbs <= 4095 = 7
            | otherwise = 8
        !res = if segment == 8
               then 0x7F
               else let !segShift = if segment < 2
                                    then 1
                                    else fromIntegral segment
                    in
                        shiftL segment 4 .|.
                            (shiftR pcmValAbs segShift .&. 0xF)
    in
        fromIntegral res `xor` mask
