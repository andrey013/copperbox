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
defs a =     text "LIBRARY" <+> text (ed_dll_name $ image_export_data a)
         $+$ text "EXPORTS"
         $+$ outputExports ( image_export_data a)

outputExports :: ExportData -> Doc
outputExports = vcat . map text . ed_name_table
