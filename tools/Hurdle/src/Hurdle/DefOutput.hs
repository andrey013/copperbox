{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.DefOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  
--
-- Print in .def format
--
--------------------------------------------------------------------------------

module Hurdle.DefOutput where

import Hurdle.Datatypes

import Text.PrettyPrint.HughesPJ

printDef :: Image -> IO ()
printDef = putStr . defText

defText :: Image -> String
defText = renderStyle (Style PageMode 80 1.5) . defs


defs :: Image -> Doc
defs = maybe failureMessage exportData . image_export_data


exportData :: ExportData -> Doc
exportData a = 
        text "LIBRARY" <+> text (ed_dll_name a)
    $+$ text "EXPORTS"
    $+$ (vcat $ map text $ ed_name_table a)


failureMessage :: Doc
failureMessage = 
        text "--- ERROR - .edata section not found in file ---"
    $+$ text "--- please use pexports for this file...     ---"           

