{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.GlyphList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Generate Wumpus files from the glyphlist.
--
-- Literate Haskell is generated rather than plain Haskell, there 
-- is no technical benefit to this it just means word counts of 
-- the Wumpus source can more confortably ignore auto-generated
-- code.
--
--------------------------------------------------------------------------------

module Wumpus.FontKit.GlyphList
  ( 
    gen_GlyphNamesModule 
  , gen_GlyphIndicesModule

  ) where

import Wumpus.FontKit.GlyphListParser
import Wumpus.FontKit.CodeGen

import Wumpus.Basic.Utils.FormatCombinators

import qualified Data.IntMap    as IntMap
import qualified Data.Map       as Map
import Data.Time


--------------------------------------------------------------------------------
-- 

gen_GlyphNamesModule :: [(String,UCode)] -> ZonedTime -> Doc
gen_GlyphNamesModule xs ztime = 
    vcat [ wallDirective, empty, modu_comment, empty
         , modu_start
         , empty
         , modu_imports
         , empty
         , table_body 
         , empty
         ]
  where
    qmodname     = "Wumpus.Core.Text.GlyphNames"
    year_string  = yearName ztime
    descr_body   = [ "Map of Unicode code points to PostScript glyph names."
                   , ""
                   , "\\*\\* This file is auto-generated. \\*\\*" ]
    
    modu_comment = haddockModuleBlock qmodname year_string descr_body ztime
    modu_start   = moduleDecl qmodname ["ps_glyph_names"]
    modu_imports = lhspre $ text "import Data.IntMap ( IntMap, fromAscList )"
    table_body   = gen_PSGlyphNames $ buildGlyphNames xs


buildGlyphNames :: [(String,UCode)] -> IntMap.IntMap String
buildGlyphNames = foldr fn IntMap.empty
  where
    fn (s,ixs) acc = foldr (\i a -> IntMap.insert i s a) acc ixs

gen_PSGlyphNames :: IntMap.IntMap String -> Doc
gen_PSGlyphNames im =
    vcat [ haddockComment doc_string, empty, type_sig, fun_decl ]
  where
    doc_string = [ "Index table mapping from Unicode code point to the" 
                 , "corresponding PostScript name defined by the Adobe"
                 , "glyphlist." ]

    type_sig   = lhspre $ text "ps_glyph_names :: IntMap String"
    fun_decl   = vconcat (lhspre $ text "ps_glyph_names = fromAscList $")
                         (gen_psGlyphTable $ IntMap.toAscList im)


gen_psGlyphTable :: [(Int,String)] -> Doc
gen_psGlyphTable = pairList ix (dquotes . text)
  where
    ix i = text "0x" <> hex4 i


--------------------------------------------------------------------------------
-- Glyph indices

gen_GlyphIndicesModule :: [(String,UCode)] -> ZonedTime -> Doc
gen_GlyphIndicesModule xs ztime = 
    vcat [ wallDirective, empty, modu_comment, empty
         , modu_start
         , empty
         , modu_imports
         , empty
         , table_body 
         , empty
         ]
  where
    qmodname     = "Wumpus.Core.Text.GlyphIndices"
    year_string  = yearName ztime
    descr_body   = [ "Map of PostScript glyph names to Unicode code points."
                   , ""
                   , "\\*\\* This file is auto-generated. \\*\\*" ]
    
    modu_comment = haddockModuleBlock qmodname year_string descr_body ztime
    modu_start   = moduleDecl qmodname ["ps_glyph_indices"]
    modu_imports = lhspre $ text "import Data.Map ( Map, fromAscList )"
    table_body   = gen_PSGlyphIndices $ buildGlyphIndices xs



buildGlyphIndices :: [(String,UCode)] -> Map.Map String Int
buildGlyphIndices = foldr fn Map.empty
  where
    fn (s,ixs) acc = foldr (\i a -> Map.insert s i a) acc ixs


gen_PSGlyphIndices :: Map.Map String Int -> Doc
gen_PSGlyphIndices mm =
    vcat [ haddockComment doc_string, empty, type_sig, fun_decl ]
  where
    doc_string = [ "Lookup table mapping from PostScript glyph name"
                 , "to the corresponding Unicode code point." ]

    type_sig   = lhspre $ text "ps_glyph_indices :: Map String Int"
    fun_decl   = vconcat (lhspre $ text "ps_glyph_indices = fromAscList $")
                         (gen_psIndexTable $ Map.toAscList mm)


gen_psIndexTable :: [(String,Int)] -> Doc
gen_psIndexTable = pairList (dquotes . text) ix
  where
    ix i = text "0x" <> hex4 i 
