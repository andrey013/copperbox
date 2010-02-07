{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Prim
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Primitive parsers - charactors, numbers...
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Prim 
  (
    char
  , anyChar
  , text
  , string
  , cstring
  , w8Zero
  , getBytes

  -- * Word - big endian 
  , word16be
  , word24be
  , word32be
  , word64be
  
  -- * Word - little endian  
  , word16le
  , word24le
  , word32le

  -- * Int 
  , int8

  -- * Int - big endian
  , int16be
  , int32be

  -- * Int - little endian  
  , int16le
  , int32le

  , ieeeFloatSP

  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.IEEEFloat
import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Utils

import Control.Applicative
import Data.Char
import Data.Int
import Data.Word



-- | Attempt to parse the supplied single character (the supplied 
-- char must be in the ASCII range 0-255).
--
-- If the parse succeeds return the char, otherwise a parse-error
-- will be thrown with 'reportError'.
--
char :: Char -> GenKangaroo ust Char
char ch = satisfy (==chi) >> return ch 
  where 
    chi = fromIntegral $ ord ch 


-- | Parse any single character. The parser consumes one byte and
-- uses 'chr' to convert it.
--
anyChar :: GenKangaroo ust Char
anyChar = (chr . fromIntegral) <$> word8 


-- | Parse a string of the supplied length @n@.
--
-- If @n@ is less than or equal to zero the empty string is 
-- returned.
--
text :: Int -> GenKangaroo ust String
text i | i <= 0    = return ""
       | otherwise = count i anyChar

-- | Parse the supplied string. All characters should be within 
-- the range 0-255.
-- 
-- If the parse succeeds return the char, otherwise a parse-error
-- will be thrown with 'reportError'.
--
string :: String -> GenKangaroo ust String
string = (substError `flip` "string" ) . mapM char 
 

-- | Parse a null-terminated C-style string.
--
cstring :: GenKangaroo ust String
cstring = manyTill anyChar w8Zero

-- | Parse the literal @0x00@.
--
w8Zero :: GenKangaroo ust Word8
w8Zero = satisfy (==0)


-- | Get @n@ bytes. 
-- 
-- If @n@ is less than or equal to zero an empty list is returned.
--
getBytes :: Integral a => a -> GenKangaroo ust [Word8]
getBytes i | i < 0     = return []
           | otherwise = count (fromIntegral i) word8


--------------------------------------------------------------------------------
-- Data.Word 

-- | Parse a Word16 in big endian form.
--
word16be   :: GenKangaroo ust Word16
word16be   = w16be     <$> word8 <*> word8  

-- | Parse a \"Word24\" in big endian form.
-- 
-- 3 bytes are read - the answer is returned as a Word32.
--
word24be   :: GenKangaroo ust Word32
word24be   = w32be 0 <$> word8 <*> word8 <*> word8

-- | Parse a Word32 in big endian form.
--
word32be   :: GenKangaroo ust Word32
word32be   = w32be     <$> word8 <*> word8 <*> word8 <*> word8

-- | Parse a Word64 in big endian form.
--
word64be   :: GenKangaroo ust Word64
word64be   = w64be <$> word8 <*> word8 <*> word8 <*> word8
                   <*> word8 <*> word8 <*> word8 <*> word8


-- | Parse a Word16 in little endian form.
--
word16le   :: GenKangaroo ust Word16
word16le   = w16le     <$> word8 <*> word8  

-- | Parse a \"Word24\" in little endian form.
--
-- 3 bytes are read - the answer is returned as a Word32.
--
word24le   :: GenKangaroo ust Word32
word24le   = w24le     <$> word8 <*> word8 <*> word8


-- | Parse a Word32 in little endian form.
--
word32le   :: GenKangaroo ust Word32
word32le   = w32le     <$> word8 <*> word8 <*> word8 <*> word8


--------------------------------------------------------------------------------
-- Data.Int

-- | Parse a single byte, returning it as an Int8.
-- 
-- The conversion from a byte (0-255) to an Int8 uses the Prelude 
-- function 'fromIntegral'. 
-- 
-- The conversion is summarized as:
--
-- > 0..127   = 0..127
-- > 128      = -128
-- > 129      = -127
-- > 130      = -126
-- > ...
-- > 254      = -2
-- > 255      = -1   
-- >
-- > wtoi :: Word8 -> Int8
-- > wtoi i | i < 128   = i
-- >        | otherwise = -128 + (clearBit i 7)
--
int8 :: GenKangaroo ust Int8
int8 = fromIntegral <$> word8


-- | Parse an Int16 in big endian form.
-- 
-- The ans is parsed as a Word16 (big endian) then converted to 
-- an Int16 using the Prelude function 'fromIntegral'.
--
int16be   :: GenKangaroo ust Int16
int16be   = i16be <$> word8 <*> word8
  

-- | Parse an Int32 in big endian form.
-- 
-- The ans is parsed as a Word32 (big endian) then converted to 
-- an Int32 using the Prelude function 'fromIntegral'.
--
int32be   :: GenKangaroo ust Int32
int32be   = i32be <$> word8 <*> word8 <*> word8 <*> word8

                         
-- | Parse an Int16 in little endian form.
-- 
-- The ans is parsed as a Word16 (little endian) then converted 
-- to an Int16 using the Prelude function 'fromIntegral'.
--
int16le   :: GenKangaroo ust Int16
int16le   = i16le <$> word8 <*> word8
                         
-- | Parse an Int32 in little endian form.
-- 
-- The ans is parsed as a Word32 (little endian) then converted 
-- to an Int32 using the Prelude function 'fromIntegral'.
--
int32le   :: GenKangaroo ust Int32
int32le   = i32le <$> word8 <*> word8 <*> word8 <*> word8


-- | Parse an 4-byte IEEE single precision float.
-- 
-- NOTE - THIS FUNCTION IS UNTESTED!
--
ieeeFloatSP :: Fractional a => GenKangaroo ust a
ieeeFloatSP = unpackIEEESingle <$> word8 <*> word8 <*> word8 <*> word8



