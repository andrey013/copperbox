{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.Syntax
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Parser for output from Windows @dir@ command.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.Syntax 
  (
    Directory(..)
  , DirContent(..)
  , DateTime(..)

  ) where


data Directory = Directory
    { dir_pathto        :: String
    , dir_content       :: [DirContent]
    }
  deriving (Eq,Show)

data DirContent = SubDir   DateTime   String
                | File     DateTime   Integer String
  deriving (Eq,Show)

data DateTime = DateTime
    { dt_year   :: !Int 
    , dt_month  :: !Int 
    , dt_day    :: !Int
    , dt_hour   :: !Int
    , dt_min    :: !Int
    }
  deriving (Eq,Show)
