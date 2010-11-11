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
  where

import Wumpus.FontKit.GlyphListParser

import Wumpus.Basic.Utils.FormatCombinators

import qualified Data.IntMap as IntMap
import Data.Time


remap :: GlyphList -> IntMap.IntMap String
remap = foldr step IntMap.empty
  where
    step (s,ns) acc = foldr (\i a -> IntMap.insert i s a) acc ns


gen_GlyphListModule :: [(String,UCode)] -> ZonedTime -> Doc
gen_GlyphListModule xs ztime = 
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
    
    modu_comment = gen_haddockModuleBlock qmodname year_string descr_body ztime
    modu_start   = gen_moduleDecl qmodname ["ps_glyph_names"]
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
gen_psGlyphTable []     = lhspre $ text "    [ ]"
gen_psGlyphTable (x:xs) = 
    vcat [ first_elem,  rest_elems, lhspre $ text "   ]" ]
  where
    first_elem = lhspre $ text "    [" <> fn x
    rest_elems = vcat $ map (\a -> lhspre $ text "    ," <> fn a) xs

    fn (i,s)   = let ix = text "0x" <> hex4 i
                 in lparen <+> ix <> comma <+> (dquotes $ text s) <+> rparen


--------------------------------------------------------------------------------

wallDirective :: Doc 
wallDirective = text "{-# OPTIONS -Wall #-}"


gen_haddockModuleBlock :: String -> String -> [String] -> ZonedTime -> Doc
gen_haddockModuleBlock qmodname year_string descr_body ztime = 
    vcat [ dashLineSep, startHaddock empty
         , modu_line, copyright, license
         , contHaddock empty
         , maintainer, stability, portability
         , contHaddock empty
         , vcat $ map (contHaddock . text) descr_body
         , contHaddock empty
         , contHaddock (text "Generated -" <+> timeStamp ztime) 
         , contHaddock empty, dashLineSep ]
  where
    modu_line   = haddockModuleProp "Module" qmodname
    copyright   = haddockModuleProp "Copyright" 
                                    ("(c) Stephen Tetley " ++ year_string)
    license     = haddockModuleProp "License" "BSD3"
    maintainer  = haddockModuleProp "Maintainer" 
                                    "Stephen Tetley <stephen.tetley@gmail.com>"
    stability   = haddockModuleProp "Stability" "unstable"
    portability = haddockModuleProp "Portability" "GHC"

    

gen_moduleDecl :: String -> [String] -> Doc
gen_moduleDecl qmodname exports = 
    vcat [ modu_prefix, open_paren, exportList exports, modu_suffix ]
  where
    modu_prefix = lhspre $ text "module" <+> text qmodname
    open_paren  = lhspre $ text "  ("
    modu_suffix = lhspre $ text "  ) where"

exportList :: [String] -> Doc
exportList [] = empty
exportList (e:es) = vcat $ (lhspre $ text "    " <+> text e) : rest
  where
    rest = map (\s -> lhspre $ text "  ," <+> text s) es

lhspre :: Doc -> Doc
lhspre d = char '>' <+> d

haddockComment :: [String] -> Doc
haddockComment []     = empty
haddockComment (s:ss) = 
    vcat $ startHaddock (text s) : map (contHaddock . text) ss 

startHaddock :: Doc -> Doc
startHaddock d = lhspre $ text "-- |" <+> d

contHaddock :: Doc -> Doc
contHaddock d = lhspre $ text "--" <+> d


dashLineSep :: Doc
dashLineSep = lhspre $ text (replicate 78 '-')

haddockModuleProp :: String -> String -> Doc
haddockModuleProp name val = contHaddock $ 
   fillStringR 12 name <+> char ':' <+> space <+> text val



timeStamp :: ZonedTime -> Doc
timeStamp zt = local_day <+> local_time
  where
    loc_time    = zonedTimeToLocalTime zt
    local_time  = timeOfDay             $ localTimeOfDay loc_time
    local_day   = text $ showGregorian  $ localDay loc_time

timeOfDay :: TimeOfDay -> Doc
timeOfDay t = 
    (pad2 $ todHour t) <> char ':' <> (pad2 $ todMin t)
  where
  
    pad2 :: Int -> Doc
    pad2 i | i < 10    = char '0' <> text (show i)
           | otherwise = text (show i)  


yearName :: ZonedTime -> String
yearName zt = show yr
  where 
    (yr,_,_) = toGregorian $ localDay $ zonedTimeToLocalTime zt

