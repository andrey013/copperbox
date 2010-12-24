{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Internal.GSFontMap
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- GhostScript Font map.
--
-- GhostScript aliases the /Core 14/ PostScript fonts to fonts
-- it can freely distribute. This module provides aliases to 
-- Wumpus so the font loader can find the equivalent GhostScript
-- files to the Core 14 set.
--
-- The data in this file matches GhostScript 8.63. Other versions
-- of GhostScript may need different aliases.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Internal.GSFontMap
  (
   
    GSFontMap(..)
  , gsMetricsFile
  , gsFontAlias
  , gsVersionNumber
  , ghostscript_fontmap_8_54

  ) where


import Data.Map ( Map )
import qualified Data.Map as Map


-- | GhostScript version that the aliases were derived from.
-- 
-- ghostscript_version :: String
-- ghostscript_version = "gs8.54"


-- | A map from standard /Adode PostScript/ font name to the
-- equivalent GhostScript font and AFM file name.
--
-- It is expected that all GhostScript AFM files will be located
-- in the same directory.
--
data GSFontMap = GSFontMap
      { ghostscript_version :: String
      , ghostscript_fontmap :: Map String (String, FilePath)
      }


-- | Get the @.afm@ metrics file.
--
-- Note this return only the file name and not the path to it.
-- The full path must be resolved in client code.
--
gsMetricsFile :: GSFontMap -> String -> Maybe FilePath
gsMetricsFile mp name = fmap snd $ Map.lookup name (ghostscript_fontmap mp)

-- | Get the GhostScript font name alias.
--
gsFontAlias :: GSFontMap -> String -> Maybe String
gsFontAlias mp name = fmap fst $ Map.lookup name (ghostscript_fontmap mp)


-- | Get the GhostScript version number that the FontMap 
-- represents.
--
gsVersionNumber :: GSFontMap -> String
gsVersionNumber = ghostscript_version 


-- | Map from PostScript font name to the corresponding 
-- GhostScript name and file.
--
-- Naming is correct for GhostSCript version 8.54.
-- 
ghostscript_fontmap_8_54 :: GSFontMap
ghostscript_fontmap_8_54 = GSFontMap "8.54" body
  where
    body = Map.fromList $ 
            [ ("Courier",                 ("NimbusMonL-Regu",         "n022003l.afm"))
            , ("Courier-Oblique",         ("NimbusMonL-ReguObli",     "n022023l.afm"))
            , ("Courier-Bold",            ("NimbusMonL-Bold",         "n022004l.afm"))
            , ("Courier-BoldOblique",     ("NimbusMonL-BoldObli",     "n022024l.afm"))
  
            , ("Helvetica",               ("NimbusSanL-Regu",         "n019003l.afm"))
            , ("Helvetica-Oblique",       ("NimbusSanL-ReguItal",     "n019023l.afm"))
            , ("Helvetica-Bold",          ("NimbusSanL-Bold",         "n019004l.afm"))
            , ("Helvetica-BoldOblique",   ("NimbusSanL-BoldItal",     "n019024l.afm"))

            , ("Times-Roman",             ("NimbusRomNo9L-Regu",      "n021003l.afm"))
            , ("Times-Italic",            ("NimbusRomNo9L-ReguItal",  "n021023l.afm"))
            , ("Times-Bold",              ("NimbusRomNo9L-Medi",      "n021004l.afm"))
            , ("Times-BoldItalic",        ("NimbusRomNo9L-MediItal",  "n021024l.afm"))

            , ("Symbol",                  ("StandardSymL",            "s050000l.afm"))

            , ("ZapfDingbats",            ("Dingbats",                "d050000l.afm"))
            ]

