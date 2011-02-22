{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Syn.Output16
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output to 16 bit audio
--
--------------------------------------------------------------------------------


module Syn.Output16
  ( 
  -- 
    makeWav_OneChan

  ) where 

import ZWav.Datatypes

import Data.HeadStrictStream                    -- package: head-strict-stream

import Data.Binary.Put
import qualified Data.ByteString.Lazy   as LBS
import Data.Int
import Data.Word

imax :: Double
imax = fromIntegral (maxBound :: Int16)

imin :: Double 
imin = fromIntegral (minBound :: Int16)

-- bits_per_sample_ :: Word16 
-- bits_per_sample_ = 16



-- Data.Binary.Put can only put Data.Word and not Data.Int,
-- need to convert 
--
toInt :: Double -> Word16
toInt = step . clamp (-1) 1 
  where
    step :: Double -> Word16
    step d | d >= 0    = floor   (d*imax)
           | otherwise = negate $ ceiling (d*imin)

    clamp :: Double -> Double -> Double -> Double
    clamp mi ma a = min ma (max mi a)


         

makeWav_OneChan :: Int -> Stream Double -> WavFile
makeWav_OneChan stream_len strm  = WavFile total_sz fmt dat
  where
    fmt        = makeFormat 1
    sz         = fromIntegral $ 
                   (stream_len * 1 * bits_per_s) `div` 8
    total_sz   = 36 + sz
    bits_per_s = 16
    dat        = DataSubchunk sz (getStreamData stream_len strm)

makeFormat :: Int -> WavFormat 
makeFormat nc = WavFormat 
      { subchunk_size           = 16
      , audio_format            = 1                 -- 1 == PCM
      , num_channels            = fromIntegral nc   -- 1 mono, 2 stereo
      , sample_rate             = 44100
      , byte_rate               = floor $ (44100 * ncd * 16) / (8::Double)
      , block_align             = floor $ ncd * 16 / (8::Double)
      , bits_per_sample         = 16
      }
  where
    ncd = fromIntegral nc
   

getStreamData :: Int -> Stream Double -> LBS.ByteString
getStreamData sz s = runPut $ fn sz s
  where
    fn i _                  | i <= 0 = return ()
    fn n (viewl -> a :< sa) = let i = toInt a in putWord16le i >> fn (n-1) sa