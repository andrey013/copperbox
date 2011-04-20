{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.WavHeader
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes.
--
--------------------------------------------------------------------------------


module ZSyn.WavHeader
  (
    -- * Wav file representation
    WavHeader(..)

  , makeWavHeader
  , wavDataLength

  ) where

import Data.Word


-- | WavHeader
-- 
-- > 00     "RIFF"
--
-- > 04      wav_size         :: Word32  -- file_size - 8 bytes
--
-- > 08     "WAVE"
-- > 12     "fmt "
-- > 16     0x10 (16) header sub chunk size for PCM data
-- 
-- > 20     audio_format      :: Word16   -- 1 - PCM, others compressed
--
-- > 22     num_channels      :: Word16   -- 1, 2 etc.
--
-- > 24     sample_rate       :: Word32   -- 8000, 44100 etc.
--
-- > 28     byte_rate         :: Word32   -- SR * num_chans * (bits_per_s/8)
-- 
-- > 32     block_align       :: Word16   -- num_chans * (bits_per_s/8)
--
-- > 34     bits_per_sample   :: Word16   -- 8, 16.
--
data WavHeader = WavHeader
      { wav_size                :: !Word32
      , wav_audio_format        :: !Word16
      , wav_num_channels        :: !Word16
      , wav_sample_rate         :: !Word32
      , wav_bits_per_sample     :: !Word16
      }
  deriving (Eq,Show) 

-- | 'makeWavHeader' : data_length * num_chans * sample_rate * bits_per_sample
--
makeWavHeader :: Int -> Word16 -> Word32 -> Word16 -> WavHeader
makeWavHeader data_len num_chans sr bps = 
   WavHeader { wav_size                 = fromIntegral sz
             , wav_audio_format         = 1
             , wav_num_channels         = num_chans
             , wav_sample_rate          = sr
             , wav_bits_per_sample      = bps
             }
  where
    sz    = (data_len * fromIntegral num_chans * width) + 44 - 8
    width = fromIntegral $ bps `div` 8

wavDataLength :: WavHeader -> Int
wavDataLength h = let sz0 = fromIntegral $ subtract 36 $ wav_size h
                      sz1 = sz0 `div` (fromIntegral $ wav_num_channels h)
                  in scalebits (wav_bits_per_sample h) sz1
  where
    scalebits bits a = a `div` (fromIntegral $ bits `div` 8)
