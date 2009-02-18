{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Loca
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Loca - index to location mapping
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Loca where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils ( regionBetween )

import Data.Array.IArray ( elems, bounds, (!) ) 
import Control.Applicative

import Text.PrettyPrint.Leijen ( Doc, Pretty(..) )

data LocaStorage = UShortLS | ULongLS
  deriving (Enum,Eq,Ord,Show)

data LocaTable = LocaTable {
        storage_mode  :: LocaStorage,
        loca_offsets  :: USequence ULong
    }
  deriving (Eq,Show)

-- Note allignment issues to think about... ?

-- numglyphs plus1 to account for 0 being the .notAccess glyph
readLocaTable :: Short -> ULong -> Parser r LocaTable
readLocaTable 0 num_glyphs = readShortLocaTable (num_glyphs + 1)
readLocaTable _ num_glyphs = readLongLocaTable (num_glyphs + 1)


readShortLocaTable :: ULong -> Parser r LocaTable
readShortLocaTable sz = LocaTable UShortLS <$>
    usequence (fromIntegral sz) ushortAsULong
  where
    ushortAsULong :: Parser r ULong
    ushortAsULong = (2 *) . fromIntegral <$> ushort   

readLongLocaTable :: ULong -> Parser r LocaTable
readLongLocaTable sz = LocaTable ULongLS  <$>
    usequence (fromIntegral sz) ulong
    
    
    
instance Pretty LocaTable where
  pretty (LocaTable mode arr) = ppTable ("Loca Table " ++ modetext mode) fs 
      where
        modetext UShortLS = "(Short version)"
        modetext ULongLS  = "(Long version)"
        
        fs = let xs = elems arr in
             snd $ foldr (\e (i,a) -> (i-1, mkField i e : a)) (length xs - 1, []) xs
        
        mkField :: Int -> ULong -> Doc
        mkField i e = field (show i) 8 (integral e)      
        
        
--------------------------------------------------------------------------------


data GlyphLocations = GlyphLocations {
      glyph_count       :: Int,
      glyf_table_size   :: Int,
      location_offsets  :: USequence ULong
    }
  deriving (Eq,Show)
  
mkGlyphLocations :: Int -> USequence ULong -> GlyphLocations
mkGlyphLocations table_size arr = let (_,gtop) = bounds arr in 
    GlyphLocations (gtop+1) table_size arr

glyfLoca :: Int -> GlyphLocations -> Region
glyfLoca i (GlyphLocations c sz arr) 
    | i+1 == c  = let start = fromIntegral $ arr!i in (start, sz - start)
    | otherwise = regionBetween (fromIntegral $ arr!i)  (fromIntegral $ arr!(i+1))
                        
     
    