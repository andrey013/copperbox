{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint.HexDump
-- Copyright   :  (c) Stephen Tetley 2009-2010
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
  
  , oxhex
  , oxhex2
  , oxhex4
  , oxhex8

  , hexdump
  , hexdumpA

  ) where


import Text.PrettyPrint.JoinPrint.Core

import Data.Array.IO
import Data.Char
import Data.List ( unfoldr )
import Data.Word
import Numeric

import Prelude hiding ( length )
import qualified Prelude as Pre


asterix :: String -> Doc
asterix = text . map (const '*')




-- | 'hex' : @i -> Doc@
-- 
-- Print @i@ as hexadecimal, no zero padding. 
--
-- Negative numbers are printed as a string of asterisks.
-- 
hex :: Integral a => a -> Doc
hex i | i >= 0    = text $ showHex i []
      | otherwise = asterix $ showHex (abs i) []



-- | Print a Word8 as a 2-digit hex number.
--
hex2 :: Word8 -> Doc
hex2 = padl 2 '0' . text . ($ []) . showHex

-- | Print a Word16 as a 4-digit hex number.
--
hex4 :: Word16 -> Doc
hex4 = padl 4 '0' . text . ($ []) . showHex

-- | Print a Word32 as a 8-digit hex number.
--
hex8 :: Word32 -> Doc
hex8 = padl 8 '0' . text . ($ []) . showHex

-- | 'oxhex' : @pad-length * i -> Doc@
--
-- Print @i@ in hexadecimal, padding with \'0\' to the supplied 
-- @pad-length@ and prefixing with \"0x\".
--
-- Negative numbers are printed as a string of asterisks.
-- 
oxhex  :: Integral a => Int -> a -> Doc
oxhex plen i 
    | i >= 0    = text "0x" <> padl plen '0' (text $ showHex i [])
    | otherwise = text "0x" <> padl plen '*' (asterix $ showHex (abs i) [])

-- | Print a Word8 as a 2-digit hex number prefixed with \"0x\".
--
oxhex2 :: Word8 -> Doc
oxhex2 = (text "0x" <>) . hex2

-- | Print a Word16 as a 4-digit hex number prefixed with \"0x\".
--
oxhex4 :: Word16 -> Doc
oxhex4 = (text "0x" <>) . hex4

-- | Print a Word32 as a 8-digit hex number prefixed with \"0x\".
--
oxhex8 :: Word32 -> Doc
oxhex8 = (text "0x" <>) . hex8

--------------------------------------------------------------------------------

hexdumpA :: Int -> Int -> IOUArray Int Word8 -> IO VDoc 
hexdumpA start end arr = getBounds arr >>= next 
  where
    next (s,e) = lineSpans c1_width (segment16Ixs (max s start) (min e end)) arr
    c1_width   = 2 +  sizeHexStr end

lineSpans :: Int -> [(Int,Int)] -> IOUArray Int Word8 -> IO VDoc 
lineSpans _        []     _   = return $ vcat []
lineSpans c1_width (x:xs) arr = do 
    w8s <- spanArr x arr
    let vdoc = vcat [firstLine (lineStart $ fst x) w8s]
    docTail xs vdoc
  where
    docTail []             vdoc = return vdoc
    docTail (ix@(s,_):ixs) vdoc = do { w8s <- spanArr ix arr
                                     ; let d = tailLine (lineStart s) w8s
                                     ; docTail ixs (vdoc `vsnoc` d) }

    firstLine                   = hexLine True  c1_width
    tailLine                    = hexLine False c1_width
                                     
        


-- This would be better if it didn't need a list in the first 
-- place (i.e. it could use an array directly)...

hexdump :: Int -> Int -> [Word8] -> VDoc
hexdump start end bs = hexdump2 start end segs  where
    segs = segment16 (16 - (start `mod` 16)) bs

hexdump2 :: Int -> Int -> [[Word8]] -> VDoc
hexdump2 start end segs = 
    vcat $ aZipWith (hexLine True c1_width, hexLine False c1_width) 
                    index_nums
                    segs
  where
    c1_width   = 2 +  sizeHexStr end
    index_nums = lineNumbers start end


sizeHexStr :: Int -> Int
sizeHexStr n = Pre.length $ showHex (abs n) []

-- zipWith an \anacrusis\ on the first element.
--
aZipWith :: (a -> b -> c, a -> b -> c) -> [a] -> [b] -> [c]
aZipWith (f,g) (x:xs) (y:ys) = f x y : zipWith g xs ys
aZipWith _     _      _      = []


--------------------------------------------------------------------------------

type Width   = Int
type LineNum = Int 

hexLine :: Bool -> Width -> LineNum -> [Word8] -> Doc 
hexLine is_initial c1_max n xs = c1 <+> empty <+> c2 <+> c3
  where
    c1  = padl c1_max ' ' (hex n)
    c2  = columnPad is_initial (16*3)   $ hsep $ map hex2 xs
    c3  = columnPad is_initial 16       $ text $ map printable xs


-- Default is to pad to the right...
--
columnPad :: Bool -> Width -> Doc -> Doc
columnPad pad_left w d = step $ length d where
    step l | l < w = if pad_left then padl w ' ' d else padr w ' ' d
    step _         = d       
                   

lineNumbers :: Int -> Int -> [Int]
lineNumbers s e = unfoldr phi $ lineStart s where
   phi n | n > e     = Nothing
         | otherwise = Just (n,n+16) 

lineEnd :: Int -> Int
lineEnd l = (l `div` 16) * 16 + 15


lineStart :: Int -> Int
lineStart l = (l `div` 16) * 16


segment16Ixs :: Int -> Int -> [(Int,Int)]
segment16Ixs start end = unfoldr phi start where
  phi i | i > end    = Nothing
        | otherwise  = let e1 = lineEnd i in Just ((i, min end e1), e1+1)


-- Show 16 bytes per line...
segment16 :: Int -> [a] -> [[a]]
segment16 initial ls = let (top,rest) = splitAt initial ls
                       in top : unfoldr phi rest
  where
    phi [] = Nothing
    phi cs = let (xs,rest) = splitAt 16 cs in Just (xs,rest)


spanArr :: (Int,Int) -> IOUArray Int Word8 -> IO [Word8]
spanArr (l,u) arr = rstep u [] where
  rstep i acc | i < l = return acc
  rstep i acc         = do { e <- readArray arr i ; rstep (i-1) (e:acc ) }

printable :: Word8 -> Char
printable = fn . chr . fromIntegral where 
  fn c | isPrint c = c
       | otherwise = '.'
