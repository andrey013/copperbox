{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Internal.SimpleFormat
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Simple formating combinators.
--
--------------------------------------------------------------------------------

module ZMidi.Core.Internal.SimpleFormat
  (

    Doc
  , width
  , output

  , sep
  , ssep
  , char
  , text
  , multiply

  , padl
  , padr
  , hex2  
  , hex4
  , integral

  ) where

import Data.Monoid
import Data.Word
import Numeric

-- Strings are represented as Hughes lists 
--
-- ShowS is a Hughes list representation specialized to Strings
--
type H a = [a] -> [a]

type HString = H Char


spaceH :: Int -> HString 
spaceH n = showString $ replicate n ' '


fromH :: H a -> [a]
fromH = ($ [])

-- | Docs represent a single line - they should not contain 
-- newlines...
--
data Doc = Doc { width :: !Int, doch :: HString }

doc :: String -> Doc
doc s = Doc (length s) (showString s) 

output :: Doc -> String
output = fromH . doch



instance Monoid Doc where
  mempty                        = Doc 0 id
  Doc i1 f1 `mappend` Doc i2 f2 = Doc (i1+i2) (f1 . f2)


infixr 6 `sep`

sep :: Doc -> Doc -> Doc
sep = mappend

infixr 6 `ssep`

ssep :: Doc -> Doc -> Doc
ssep (Doc i1 f1) (Doc i2 f2) = Doc (1+i1+i2) (f1 . (' ':) . f2)


char :: Char -> Doc 
char c = Doc 1 (c:)

text :: String -> Doc
text = doc 

multiply :: Int -> Char -> Doc
multiply n c = Doc n (showString $ replicate n c)


padl :: Int -> Doc -> Doc
padl i d@(Doc n f) | i > n     = Doc i (spaceH (i-n) . f)
                   | otherwise = d

padr :: Int -> Doc -> Doc
padr i d@(Doc n f) | i > n     = Doc i (f . spaceH (i-n))
                   | otherwise = d


hex2 :: Word8 -> Doc
hex2 n | n < 0x10  = Doc 2 (('0' :) . showHex n)
       | otherwise = Doc 2 (showHex n)  

hex4 :: Word16 -> Doc
hex4 n | n < 0x10   = Doc 4 (('0':) . ('0':) . ('0':) . showHex n)
       | n < 0x100  = Doc 4 (('0':) . ('0':) . showHex n)
       | n < 0x1000 = Doc 4 (('0':) . showHex n)
       | otherwise = Doc 4 (showHex n)  


integral :: Integral a => a -> Doc
integral = doc . show 