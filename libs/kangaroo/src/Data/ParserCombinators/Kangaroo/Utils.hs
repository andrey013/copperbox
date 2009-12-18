{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils...
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Utils 
  ( 
    (<:>)
  
  -- * Specs
  , oo
  , ooo
  , oooo

  -- * numbers from Word8 
  , w16be
  , w32be

  , w16le
  , w32le
  , w64be

  , i16be
  , i32be

  , i16le
  , i32le
  
  , shiftL8
  , shiftL16
  , shiftL24
  , shiftL32
  , shiftL40
  , shiftL48
  , shiftL56
  
  -- * Hex printing
  , hex2

  ) where


import Control.Applicative

import Data.Bits
import Data.Int
import Data.Word
import Numeric

infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


-- specs - defined in my package data-aviary but defined here to 
-- avoid a dependency

-- | Compose an arity 1 function with an arity 2 function.
-- B1 - blackbird
oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g  



--------------------------------------------------------------------------------



w16le :: Word8 -> Word8 -> Word16
w16le a b = fromIntegral a + (shiftL8 b)

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = fromIntegral a + (shiftL8 b) + (shiftL16 c) + (shiftL24 d)      


-- Woah! The integer ones don't look right - what about the sign?

i16le :: Word8 -> Word8 -> Int16
i16le a b = fromIntegral $ w16le a b
              
i32le :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
i32le a b c d  = fromIntegral $ w32le a b c d                


i16be :: Word8 -> Word8 -> Int16
i16be a b = fromIntegral $ w16be a b
                            
i32be :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
i32be a b c d = fromIntegral $ w32be a b c d


w16be :: Word8 -> Word8 -> Word16
w16be a b = (shiftL8 a) + fromIntegral b
     
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = (shiftL24 a) + (shiftL16 b) + (shiftL8 c) + fromIntegral d

-- To do...
w64be :: Word8 -> Word8 -> Word8 -> Word8 -> 
           Word8 -> Word8 -> Word8 -> Word8 -> Word64
w64be a b c d e f g h = a' + b' + c' + d' + e' + f' + g' + h' where
    a' = (fromIntegral a) `shiftL` 56
    b' = (fromIntegral b) `shiftL` 48
    c' = (fromIntegral c) `shiftL` 40
    d' = (fromIntegral d) `shiftL` 32
    e' = (fromIntegral e) `shiftL` 24
    f' = (fromIntegral f) `shiftL` 16
    g' = (fromIntegral g) `shiftL` 8
    h' = (fromIntegral h) 


shiftL8 :: (Bits b, Integral b) => Word8 -> b
shiftL8 = (`shiftL` 8) . fromIntegral

shiftL16 :: (Bits b, Integral b) => Word8 -> b
shiftL16 = (`shiftL` 16) . fromIntegral

shiftL24 :: (Bits b, Integral b) => Word8 -> b
shiftL24 = (`shiftL` 24) . fromIntegral

shiftL32 :: (Bits b, Integral b) => Word8 -> b
shiftL32 = (`shiftL` 32) . fromIntegral

shiftL40 :: (Bits b, Integral b) => Word8 -> b
shiftL40 = (`shiftL` 40) . fromIntegral

shiftL48 :: (Bits b, Integral b) => Word8 -> b
shiftL48 = (`shiftL` 48) . fromIntegral

shiftL56 :: (Bits b, Integral b) => Word8 -> b
shiftL56 = (`shiftL` 56) . fromIntegral




hex2 :: Integral a => a -> ShowS
hex2 a | a < 0      = showString "-ve"
       | a < 10     = showString "0x0" . showHex a
       | otherwise  = showString "0x" . showHex a 