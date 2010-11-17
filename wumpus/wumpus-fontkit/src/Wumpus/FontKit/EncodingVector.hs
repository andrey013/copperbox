{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.EncodingVector
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Generate Wumpus files for encosing vectors.
--
-- Literate Haskell is generated rather than plain Haskell, there 
-- is no technical benefit to this it just means word counts of 
-- the Wumpus source can more confortably ignore auto-generated
-- code.
--
--------------------------------------------------------------------------------

module Wumpus.FontKit.EncodingVector
  ( 

    Encoding(..)
  , gen_EncodingModule

  ) where

import Wumpus.FontKit.CodeGen

import Wumpus.Basic.Utils.FormatCombinators

import qualified Data.IntMap    as IntMap
import Data.Time


--------------------------------------------------------------------------------
-- 

data Encoding = Encoding
      { encoding_module_name    :: String 
      , encoding_module_descr   :: [String]
      , encoding_table_name     :: String 
      , encoding_table_descr    :: [String]
      }
  deriving (Eq,Show) 

gen_EncodingModule :: Encoding -> [(String,Int)] -> ZonedTime -> Doc
gen_EncodingModule enc_data xs ztime = 
    vcat [ wallDirective, empty, modu_comment, empty
         , modu_start
         , empty
         , importList import_list
         , empty
         , table_body 
         , empty
         ]
  where
    qmodname     = encoding_module_name enc_data
    decl_name    = encoding_table_name  enc_data
    year_string  = yearName ztime
    descr_body   = encoding_module_descr enc_data 
                     ++ [ "", "\\*\\* This file is auto-generated. \\*\\*" ]
    import_list  = [ "Wumpus.Core.Text.Base"
                   , ""
                   , "qualified Data.IntMap as IntMap" ]

    
    modu_comment = haddockModuleBlock qmodname year_string descr_body ztime
    modu_start   = moduleDecl qmodname [decl_name]
    table_body   = gen_PSEncodingVector enc_data $ buildEncodingVector xs


buildEncodingVector :: [(String,Int)] -> IntMap.IntMap String
buildEncodingVector = foldr (\(s,i) a -> IntMap.insert i s a) IntMap.empty


gen_PSEncodingVector :: Encoding -> IntMap.IntMap String -> Doc
gen_PSEncodingVector enc_data im =
    vcat [ haddockComment doc_string, empty, type_sig, fun_decl ]
  where
    doc_string = encoding_table_descr enc_data
    decl_name  = encoding_table_name  enc_data

    type_sig   = lhspre $ text decl_name <+> text ":: EncodingVector"
    fun_decl   = vconcat (lhspre $ text decl_name <+> text "= IntMap.fromAscList $")
                         (pairList ix (dquotes . text) $ IntMap.toAscList im)
    ix         = \i -> text "0x" <> hex4 i
