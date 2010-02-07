{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions
--
--------------------------------------------------------------------------------


module ZMidi.Utils (
    w16le
  , w32le
  , w16be
  , w32be
  , hex2
  , hex4
  , hex8
  ) where

import Data.Bits
import Data.Word
import Numeric

w16le :: Word8 -> Word8 -> Word16
w16le a b = fromIntegral a + (shiftL8 b)

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = fromIntegral a + (shiftL8 b) + (shiftL16 c) + (shiftL24 d)      

w16be :: Word8 -> Word8 -> Word16
w16be a b = (shiftL8 a) + fromIntegral b
     
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = (shiftL24 a) + (shiftL16 b) + (shiftL8 c) + fromIntegral d

shiftL8 :: (Bits b, Integral b) => Word8 -> b
shiftL8 = (`shiftL` 8) . fromIntegral

shiftL16 :: (Bits b, Integral b) => Word8 -> b
shiftL16 = (`shiftL` 16) . fromIntegral

shiftL24 :: (Bits b, Integral b) => Word8 -> b
shiftL24 = (`shiftL` 24) . fromIntegral

shiftL32 :: (Bits b, Integral b) => Word8 -> b
shiftL32 = (`shiftL` 32) . fromIntegral


--------------------------------------------------------------------------------

hex2 :: Integral a => a -> ShowS
hex2 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x0" . showHex a
       | otherwise  = showString "0x"  . showHex a 


hex4 :: Integral a => a -> ShowS
hex4 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x000" . showHex a
       | a < 0x100  = showString "0x00"  . showHex a 
       | a < 0x1000 = showString "0x0"   . showHex a 
       | otherwise  = showString "0x"    . showHex a 

hex8 :: Integral a => a -> ShowS
hex8 a | a < 0          = showString "-ve"
       | a < 0x10       = showString "0x0000000" . showHex a
       | a < 0x100      = showString "0x000000"  . showHex a 
       | a < 0x1000     = showString "0x00000"   . showHex a 
       | a < 0x10000    = showString "0x0000"    . showHex a 
       | a < 0x100000   = showString "0x000"     . showHex a 
       | a < 0x1000000  = showString "0x00"      . showHex a 
       | a < 0x10000000 = showString "0x0"       . showHex a 
       | otherwise      = showString "0x"        . showHex a 

