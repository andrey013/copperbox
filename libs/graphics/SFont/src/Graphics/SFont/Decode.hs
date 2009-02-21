{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.SFont.Decode
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Decode operations (glyf's have a separate module)
--
--------------------------------------------------------------------------------

module Graphics.SFont.Decode where

import Graphics.SFont.ExtraSyntax
import Graphics.SFont.PrimitiveDatatypes

-- The /last/ region only has a start position, so we supply the end loc 
-- of the glyf table (glyf_table_length). This nicely works backwards hence 
-- the right fold.  
glyfLocations :: Int -> [GlyfStartLoca] -> [Region]
glyfLocations glyf_table_length = snd . foldr fn (glyf_table_length,[]) 
  where
    fn i (j,rs) = let i' = fromIntegral i in (i',(i',j-i'):rs)
    

