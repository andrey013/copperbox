{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.FlatSyntax
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- /Flat/ syntax - no nesting.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.FlatSyntax 
  (
    Directory(..)
  , DirContent(..)
  , DateTime(..)
  , SubDir(..)
  , File(..)

  , invalid_date_time

  , dirChildren
  , dirSubDirs
  , dirFiles
  , dirDateTime
  , dirIsChild  

  ) where

import Data.List

data Directory = Directory
    { dir_pathto        :: String
    , dir_content       :: [DirContent]
    }
  deriving (Eq,Show)

data DirContent = D SubDir
                | F File
  deriving (Eq,Show)

data SubDir = SubDir DateTime String
  deriving (Eq,Show)

data File = File DateTime Integer String
  deriving (Eq,Show)

data DateTime = DateTime
    { dt_year   :: !Int 
    , dt_month  :: !Int 
    , dt_day    :: !Int
    , dt_hour   :: !Int
    , dt_min    :: !Int
    }
  deriving (Eq,Show)


invalid_date_time :: DateTime
invalid_date_time = DateTime    
    { dt_year   = 1900
    , dt_month  = 1
    , dt_day    = 1
    , dt_hour   = 0
    , dt_min    = 0
    }


dirChildren :: Directory -> [DirContent]
dirChildren (Directory _ xs) = filter notLink xs
  where
    notLink (D (SubDir _ name)) = name /= "." && name /= ".."
    notLink (F {})              = True


dirSubDirs :: Directory -> [SubDir]
dirSubDirs (Directory _ xs) = foldr fn [] xs
  where
    fn (D (SubDir dt name)) ac 
        | name /= "." || name /= ".." = SubDir dt name : ac
    fn _                     ac = ac

dirFiles :: Directory -> [File]
dirFiles (Directory _ xs) = foldr fn [] xs
  where
    fn (F file) ac = file : ac
    fn _        ac = ac


-- | aka time stamp of the directory \".\"
--
dirDateTime :: Directory -> DateTime
dirDateTime (Directory _ cs) = fn cs
  where 
    fn (D (SubDir dt name):_) | name == "." = dt
    fn (_:xs)                 = fn xs
    fn []                     = invalid_date_time


dirIsChild :: String -> Directory -> Bool
dirIsChild root dir = root `isPrefixOf` (dir_pathto dir)