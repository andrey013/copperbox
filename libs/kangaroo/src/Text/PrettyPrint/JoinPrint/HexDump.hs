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
    hex2
  , hex4
  , hex8
  , hexdump

  -- TODO
  , segment16

  ) where


import Text.PrettyPrint.JoinPrint.Core

import Data.Char
import Data.List ( unfoldr )
import Data.Word
import Numeric

hex2 :: Word8 -> Doc
hex2 = padl 2 '0' . text . ($ "") . showHex


hex4 :: Word16 -> Doc
hex4 = padl 4 '0' . text . ($ "") . showHex

hex8 :: Word32 -> Doc
hex8 = padl 8 '0' . text . ($ "") . showHex

printable :: Word8 -> Char
printable = fn . chr . fromIntegral where 
  fn c | isPrint c = c
       | otherwise = '.'


hexdump :: Int -> Int -> [Word8] -> Doc
hexdump start end bs = 
    columns2 (16*3,AlignCenter) (16,AlignLeft) (<+>) hex_col char_col
  where
    startm   = start `mod` 16 
    segs     = segment16 (16-startm) bs
    char_col = map1 (padl 16 ' ')      $ map (text . map printable) segs
    
    hex_col  = map1 (padl (3*16) ' ')  $ map (hsep . map hex2) segs
    


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

data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Eq,Show)

columns2 :: (Int,Align) -> (Int,Align) 
         -> (Doc -> Doc -> Doc) 
         -> [Doc] -> [Doc] 
         -> Doc
columns2 (l1,a1) (l2,a2) op = step where
  step (x:xs)   (y:ys)   = (spad l1 x `op` spad l2 y) <$> step xs ys
  step (x:xs)   []       = (spad l1 x `op` spacer l2) <$> step xs []
  step []       (y:ys)   = (spacer l1 `op` spad l2 y) <$> step [] ys
  step []       []       = empty
  
  spad i = padr i ' '
