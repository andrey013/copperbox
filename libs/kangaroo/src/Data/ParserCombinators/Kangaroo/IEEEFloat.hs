{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.IEEEFloat
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- IEEE floats (single precision so far...)
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.IEEEFloat where

import Data.ParserCombinators.Kangaroo.Utils


import Data.Bits
import Data.Char
import Data.Word
import Numeric


const_B :: Int
const_B = 127


printBin :: (Fractional a,Ord a) => a -> ShowS
printBin a = 
    f s . showChar ' ' . f t . showChar ' ' . f u . showChar ' ' . f v
  where
    f = showIntAtBase 2 (chr . (48+))
    (s,t,u,v) = packIEEESingle a 
  
toAndFro :: (Fractional a, Ord a) => a -> a
toAndFro a = let (s,t,u,v) = packIEEESingle a in unpackIEEESingle s t u v

unpackIEEESingle :: Fractional a => Word8 -> Word8 -> Word8 -> Word8 -> a
unpackIEEESingle b24_31 b16_23 b8_15 b0_7 =  sign $ fract * (2 ^^ expo)
  where
    sign  = if b24_31 `testBit` 7 then negate else id
  
    expo  = exponent' b24_31 b16_23
  
    fract = fraction b16_23 b8_15 b0_7


exponent' :: Word8  -> Word8 -> Int
exponent' a b = (a' `shiftL` 1) + (b' `shiftR` 7) - 127
  where
    a' = fromIntegral $ (a .&. 0x7f) 
    b' = fromIntegral $ (b .&. 0x80)


iPow :: Fractional a => a -> Integer -> a
iPow = (^^)

fraction :: Fractional a => Word8 -> Word8 -> Word8 -> a
fraction b16_24 b8_15 b0_7 = 1.0 + ((fromIntegral frac) / (2 `iPow` 23))
  where
   frac :: Int 
   frac = (shiftL16 (b16_24 .&. 0x7f)) + (shiftL8 b8_15) + fromIntegral b0_7
    


packIEEESingle :: (Fractional a,Ord a) => a -> (Word8,Word8,Word8,Word8)
packIEEESingle a = (flipSign b24_31, exp_part+mant_part, b8_15, b0_7)
  where
    k     = findPosExpo $ abs a
    e     = k + const_B
    halfa = (abs a) / (2 `iPow` fromIntegral k)
    f     = expand $ halfa - 1
    
    (b24_31, exp_part)          = expoWords e
    (mant_part,b8_15, b0_7)     = mantWords f
    
    flipSign = if a > 0 then id else (`setBit` 7)


findPosExpo :: (Fractional a, Ord a) => a -> Int
findPosExpo r | r <= 0    = 0 
              | otherwise = step r 1
  where
    step r' k | r <= fromIntegral (2::Int) ^^ k = k-1
              | otherwise                       = step r' (k+1)


expand :: (Fractional a, Ord a) => a -> Word32
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
    

