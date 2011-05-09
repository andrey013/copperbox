{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercollider.SynthDef.Internal.IEEE754
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- SIngle precision IEEE 754 floats.
--
--------------------------------------------------------------------------------

module Sound.Slac.Supercollider.SynthDef.Internal.IEEE754
  (

    packSingle
  , unpackSingle  

  , printBin

  ) where

import Data.Bits
import Data.Char
import Data.Word
import Numeric
import Prelude hiding ( exponent )

const_B :: Int
const_B = 127


printBin :: Double -> ShowS
printBin a = 
    f s . showChar ' ' . f t . showChar ' ' . f u . showChar ' ' . f v
  where
    f = showIntAtBase 2 (chr . (48+))
    (s,t,u,v) = packSingle a 
  

--------------------------------------------------------------------------------
-- pack


packSingle :: Double -> (Word8,Word8,Word8,Word8)
packSingle a = (flipSign b24_31, exp_part+mant_part, b8_15, b0_7)
  where
    k     = findPosExpo $ abs a
    e     = k + const_B
    halfa = (abs a) / (2 `iPow` fromIntegral k)
    f     = expand $ halfa - 1
    
    (b24_31, exp_part)          = expoWords e
    (mant_part,b8_15, b0_7)     = mantWords f
    
    flipSign = if a > 0 then id else (`setBit` 7)


findPosExpo :: Double -> Int
findPosExpo r | r <= 0    = 0 
              | otherwise = step r 1
  where
    step r' k | r <= fromIntegral (2::Int) ^^ k = k-1
              | otherwise                       = step r' (k+1)


expand :: Double -> Word32
expand n = (`shiftR` 9) $ step n 0 id
  where
    step x ix f | x <= 0    = f (0::Word32) 
                | otherwise = let y = 1 / (2 ^^ (ix+1))
                              in if x >= y 
                                 then step (x-y) (ix+1) (f . (`setBit` (31-ix)))
                                 else step x (ix+1) f



-- 7 bits left, 1 bit right
expoWords :: Int -> (Word8,Word8)
expoWords n = (left, right)
  where
    right = if n `testBit` 0 then 128 else 0
    left  = fromIntegral $ n `shiftR` 1


mantWords :: Word32 -> (Word8,Word8,Word8)
mantWords x = (a,b,c)
  where
    c = fromIntegral $ x .&. 0xff
    b = fromIntegral $ (`shiftR` 8)  $ x .&. 0xff00
    a = fromIntegral $ (`shiftR` 16) $ x .&. 0xff0000



--------------------------------------------------------------------------------
-- Unpack

unpackSingle :: Word8 -> Word8 -> Word8 -> Word8 -> Double
unpackSingle b24_31 b16_23 b8_15 b0_7 =  sign $ fract * (2 ^^ expo)
  where
    sign  = if b24_31 `testBit` 7 then negate else id
  
    expo  = exponent b24_31 b16_23
  
    fract = fraction b16_23 b8_15 b0_7


exponent :: Word8 -> Word8 -> Int
exponent a b = (a' `shiftL` 1) + (b' `shiftR` 7) - 127
  where
    a' = fromIntegral $ (a .&. 0x7f) 
    b' = fromIntegral $ (b .&. 0x80)


iPow :: Double -> Integer -> Double
iPow = (^^)

fraction :: Word8 -> Word8 -> Word8 -> Double
fraction b16_24 b8_15 b0_7 = 1.0 + ((fromIntegral frac) / (2 `iPow` 23))
  where
   frac :: Int 
   frac = (shiftL16 (b16_24 .&. 0x7f)) + (shiftL8 b8_15) + fromIntegral b0_7
    


shiftL8 :: (Bits b, Integral b) => Word8 -> b
shiftL8 = (`shiftL` 8) . fromIntegral

shiftL16 :: (Bits b, Integral b) => Word8 -> b
shiftL16 = (`shiftL` 16) . fromIntegral
