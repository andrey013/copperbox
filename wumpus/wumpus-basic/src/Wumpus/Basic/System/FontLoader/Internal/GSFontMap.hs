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
   
    ghostscript_version  

  , GSFontMap
  , gsMetricsFile
  , gsFontAlias
  , core14_alias_table

  ) where


import Data.Map ( Map )
import qualified Data.Map as Map


-- | GhostScript version that the aliases were derived from.
-- 
ghostscript_version :: String
ghostscript_version = "gs8.54"

type GSFontMap = Map String (String, FilePath)


-- | Get the @.afm@ metrics file.
--
-- Note this return only the file name and not the path to it.
-- The full path must be resolved in client code.
--
gsMetricsFile :: GSFontMap -> String -> Maybe FilePath
gsMetricsFile mp name = fmap snd $ Map.lookup name mp

-- | Get the GhostScript font name alias.
--
gsFontAlias :: GSFontMap -> String -> Maybe String
gsFontAlias mp name = fmap fst $ Map.lookup name mp

-- | Map from PostScript font name to the corresponding 
-- GhostScript name and file.
--
core14_alias_table :: GSFontMap
core14_alias_table = Map.fromList $ 
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

