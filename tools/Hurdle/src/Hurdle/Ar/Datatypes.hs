{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Ar.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------

module Hurdle.Ar.Datatypes where

import Data.Word

data ArArchive = ArArchive 
      { ar_magic                    :: String
      , ar_header                   :: ArHeader
      , ar_symbol_table             :: ArSymbolTable
      }
  deriving Show


data ArHeader = ArHeader 
      { arh_name                    :: String
      , arh_date                    :: String
      , arh_user_id                 :: String
      , arh_group_id                :: String
      , arh_mode                    :: String
      , arh_size                    :: String
      , arh_trailer                 :: String
      }  
  deriving Show



data ArSymbolTable = ArSymbolTable 
      { symb_number_elements        :: Word32
    --  , symb_data_length            :: Word32
      }
   deriving Show






