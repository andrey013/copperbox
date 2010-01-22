{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Coff.DefOutput
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

module Hurdle.Coff.DefOutput where

import Hurdle.Coff.Datatypes

import Text.PrettyPrint.JoinPrint

printDef :: Image -> IO ()
printDef = mapM_ (putStrLn . render) . defs


defs :: Image -> [Doc]
defs = maybe failureMessage exportData . image_export_data


exportData :: ExportData -> [Doc]
exportData a = 
    text "LIBRARY" <+> text (ed_dll_name a) : text "EXPORTS"
        : (map text $ ed_name_table a)


failureMessage :: [Doc]
failureMessage = 
    [ text "--- ERROR - .edata section not found in file ---"
    , text "--- please use pexports for this file...     ---"
    ]

