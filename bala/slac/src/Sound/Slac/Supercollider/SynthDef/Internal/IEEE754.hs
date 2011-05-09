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
  , printHex
  , showBinary

  ) where

import Data.Bits
import Data.Word
import Numeric
import Prelude hiding ( exponent )


printBin :: Double -> ShowS
printBin dd = 
    sign . showString " | " . expo . showString " | " . mant
  where
    (s,t,u,v)      = packSingle dd
  
    mkBit a ix     = if testBit a ix then showChar '1' else showChar '0'
    sign           = mkBit s 7
    expo           = bits s (down 6) . showChar ' ' . bits t [7]
    mant           = bits t (down 6) . showChar ' ' . bits u (down 7) 
                                     . showChar ' ' . bits v (down 7)
    bits _ []      = id
    bits a (i:is)  = mkBit a i . bits a is

    down i | i < 0 = []
    down i         = i : down (i-1)

printHex :: Double -> ShowS
printHex dd = 
    showString "0x" . showHex s 
                    . sep . showHex t . sep . showHex u . sep . showHex v           
  where
    (s,t,u,v)      = packSingle dd
    sep            = showString " 0x"

showBinary :: Integral a => a -> ShowS
showBinary = showIntAtBase 2 fn
  where
    fn 0 = '0'
    fn 1 = '1'
    fn _ = '?'



--------------------------------------------------------------------------------
-- pack


-- Note - cannot use built-in decodeFloat / exponent as it returns 
-- answers in the wrong range.



packSingle :: Double -> (Word8,Word8,Word8,Word8)
packSingle a | a == 0 = (0,0,0,0)
packSingle a          = (flipSign b24_31, exp_part+mant_part, b8_15, b0_7)
  where
    dd          = abs a
    (k,zerodf)  = base2ExpoK dd
    e           = bias127 k
    fstr        = mantissa zerodf
    
    (b24_31, exp_part)          = expoWords e
    (mant_part,b8_15, b0_7)     = mantWords fstr
    
    flipSign = if a >= 0 then id else (`setBit` 7)



-- | Condition: 0 <= r
--
-- ans = (k,0.aaaa)
--
base2ExpoK :: Double -> (Int,Double)
base2ExpoK r | r < 1 = kmult r 0
             | r > 2 = kdiv r 0
             | otherwise = (0,fracpart r)
  where
    kmult k n | k >= 1    = (-n,fracpart k)
              | n >  127  = error $ "base2ExpoK - " ++ show r
              | otherwise = kmult (2*k) (n+1)

    kdiv k n  | k <  2    = (n, fracpart k)
              | n > 127   = error $ "base2ExpoK - " ++ show r
              | otherwise = kdiv (k / 2) (n+1)

    fracpart k = k - (fromIntegral $ floori k)

    floori :: Double -> Integer
    floori = floor



     
bias127 :: Int -> Word8
bias127 k = 127 + (fromIntegral k)

-- | Condition: 0 <= r < 1
-- 
mantissa :: Double -> Word32
mantissa r = step r 1 0
  where
    two :: Integer
    two = 2

    step :: Double -> Int -> Word32 -> Word32
    step v i str 
        | v <= 0 = str
        | otherwise = let pw   = (1 / (fromIntegral $ two^i))
                      in if pw > v then step v (i+1) str
                                   else step (v-pw) (i+1) (setBit str (23-i))  


-- | a (exponent) 7 bits left, b (mantissa) 1 bit right
--
expoWords :: Word8 -> (Word8,Word8)
expoWords e = (a, b)
  where
    a = e `shiftR` 1
    b = (e .&. 0x01) `shiftL` 7

mantWords :: Word32 -> (Word8,Word8,Word8)
mantWords x = (a,b,c)
  where
    c = fromIntegral $ x .&. 0xff
    b = fromIntegral $ (`shiftR` 8)  $ x .&. 0xff00
    a = fromIntegral $ (`shiftR` 16) $ x .&. 0x7f0000



--------------------------------------------------------------------------------
-- Unpack

-- unpack seems to work...

unpackSingle :: Word8 -> Word8 -> Word8 -> Word8 -> Double
unpackSingle 0 0 0 0 = 0.0
unpackSingle a b c d =  sign $ fract * (2 ^^ expo)
  where
    sign  = if a `testBit` 7 then negate else id
  
    expo  = exponent a b
  
    fract = fraction b c d

-- Exponent is bits [6..0] of @a@ and bit 7 of @b@
exponent :: Word8 -> Word8 -> Int
exponent a b = a' + b' - 127
  where
    a' = (`shiftL` 1) $ fromIntegral $ (a .&. 0x7f) 
    b' = if b `testBit` 7 then 1 else 0


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
