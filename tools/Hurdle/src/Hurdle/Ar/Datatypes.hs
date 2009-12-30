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
      , ar_objects                  :: [ArchiveObject]
      }
  deriving Show

data ArchiveObject = ArchiveObject
     { ar_header                    :: ArHeader
     , ar_body                      :: [Char]
     }

data ArHeader = ArHeader 
      { arh_name                    :: String
      , arh_date                    :: String
      , arh_user_id                 :: Int
      , arh_group_id                :: Int
      , arh_mode                    :: String
      , arh_size                    :: Int
      , arh_trailer                 :: String
      }  
  deriving Show



instance Show ArchiveObject where
  show (ArchiveObject h _) = "ArchiveObject " ++ show h






