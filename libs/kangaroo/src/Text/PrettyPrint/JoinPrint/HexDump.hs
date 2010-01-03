{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HexDump
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hex dumps
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.HexDump
  ( 
    hex
  , hex2
  , hex4
  , hex8
  , oxhex2
  , oxhex4
  , oxhex8

  , hexdump


  ) where


import Text.PrettyPrint.JoinPrint.Core
import Text.PrettyPrint.JoinPrint.Tabular

import Data.Char
import Data.List ( unfoldr )
import Data.Word
import Numeric

-- no zero padding
hex :: Integral a => a -> Doc
hex = text . (showHex `flip` "")

hex2 :: Word8 -> Doc
hex2 = padl 2 '0' . text . ($ "") . showHex

hex4 :: Word16 -> Doc
hex4 = padl 4 '0' . text . ($ "") . showHex

hex8 :: Word32 -> Doc
hex8 = padl 8 '0' . text . ($ "") . showHex


oxhex2 :: Word8 -> Doc
oxhex2 = (text "0x" <>) . hex2

oxhex4 :: Word16 -> Doc
oxhex4 = (text "0x" <>) . hex4

oxhex8 :: Word32 -> Doc
oxhex8 = (text "0x" <>) . hex8



printable :: Word8 -> Char
printable = fn . chr . fromIntegral where 
  fn c | isPrint c = c
       | otherwise = '.'


hexdump :: Int -> Int -> [Word8] -> Doc
hexdump start end bs = 
    columns3  col_attrs1 col_attrs2 col_attrs3 dblspace col1 col2 col3
  where
    startm    = start `mod` 16 
    segs      = segment16 (16-startm) bs
    end_width = length $ showHex end ""

    col_attrs1 = (end_width+2, AlignRight)
    col_attrs2 = (16*3, AlignCenter)
    col_attrs3 = (16, AlignLeft)

    col1       = map  (padl end_width ' ') $ index_nums
    col2       = map1 (padl 16 ' ')        $ map (text . map printable) segs
    col3       = map1 (padl (3*16) ' ')    $ map (hsep . map hex2) segs
    
    index_nums = unfoldr phi $ start - (16-startm)
    phi x  | x > end   = Nothing
           | otherwise = Just (hex x,x+16)


-- Show 16 bytes per line...
segment16 :: Int -> [a] -> [[a]]
segment16 ana ls = let (top,rest) = splitAt ana ls
                   in top : unfoldr phi rest
  where
    phi [] = Nothing
    phi cs = let (xs,rest) = splitAt 16 cs in Just (xs,rest)

map1 :: (a -> a) -> [a] -> [a] 
map1 f (a:xs) = f a : xs
map1 _ []     = []
