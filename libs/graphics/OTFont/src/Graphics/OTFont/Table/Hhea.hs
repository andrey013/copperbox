{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Hhea
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hhea - horizontal header
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Hhea where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Control.Applicative
import Data.Typeable

import Text.PrettyPrint.Leijen ( Pretty(..) )

data HheaTable = HheaTable { 
      hhea_version_num        :: Fixed,
      ascender                :: FWord,
      descender               :: FWord,
      line_gap                :: FWord,
      advance_width_max       :: UFWord,
      min_left_side_bearing   :: FWord,
      min_right_side_bearing  :: FWord,
      x_max_extent            :: FWord,
      caret_slope_rise        :: Short,
      caret_slope_run         :: Short,
      caret_offset            :: Short,
      metric_data_format      :: Short,
      number_of_h_metrics     :: UShort
      
    }
  deriving (Eq,Show,Typeable)

readHheaTable :: Monad m => ParserT r m HheaTable
readHheaTable = HheaTable <$> 
        fixed   <*> fword   <*> fword   <*> fword
    <*> ufword  <*> fword   <*> fword   <*> fword
    <*> short   <*> short   <*> (short  <*  padS4)   
    <*> short
    <*> ushort
  where 
    padS4 = (\ _ _ _ _ -> ()) <$> short <*> short <*> short <*> short
    
     
instance Pretty HheaTable where
  pretty t = ppTable "hhea Table"
      [ field "version_num"             24 (pretty   $ hhea_version_num t)
      , field "ascender"                24 (pretty   $ ascender t)
      , field "descender"               24 (pretty   $ descender t)
      , field "line_gap"                24 (pretty   $ line_gap t)
      , field "advance_width_max"       24 (pretty   $ advance_width_max t)
      , field "min_left_side_bearing"   24 (pretty   $ min_left_side_bearing t)
      , field "min_right_side_bearing"  24 (pretty   $ min_right_side_bearing t)
      , field "x_max_extent"            24 (pretty   $ x_max_extent t)
      , field "caret_slope_rise"        24 (integral $ caret_slope_rise t)
      , field "caret_slope_run"         24 (integral $ caret_slope_run t)
      , field "caret_offset"            24 (integral $ caret_offset t)
      , field "metric_data_format"      24 (integral $ metric_data_format t)
      , field "number_of_h_metrics"     24 (integral $ number_of_h_metrics t)
      ]

                            