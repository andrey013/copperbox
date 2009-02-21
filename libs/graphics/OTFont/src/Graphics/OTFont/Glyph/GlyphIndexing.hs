{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Glyph.GlyphIndexing
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ..
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Glyph.GlyphIndexing where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Table.Cmap

import Data.Array.Unboxed ( (!) )


findGlyphIndex :: UShort -> CmapSubtable -> Int
findGlyphIndex c (Format4 _ _ ends strt delt ofst gixs) = 
    let i     = findEndIndex c ends
        ro    = ofst!i
        gi    = if ro == 0 then fromIntegral $ c `plus` (delt!i) 
                           else fromIntegral $ ro `div` 2 + (c - strt!i)
    in fromIntegral $ gixs!gi 
  where
    plus :: UShort -> Short -> Short
    plus a b = (fromIntegral a) + b

findGlyphIndex _ _ = error $ "findGlyphIndex - unhandled subtable"
    
-- do a left to right traversal instead of a binary search    
findEndIndex :: UShort -> USequence UShort -> Int
findEndIndex c arr = step 0 where
    step i | arr!i >= c = i
           | otherwise  = step (i+1)  
    
    
    