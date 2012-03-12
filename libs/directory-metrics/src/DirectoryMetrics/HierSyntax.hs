{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.HierSyntax
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- /Hier/-archical syntax - nesting.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.HierSyntax 
  (
    Directory(..)
  , File(..)            -- re-export
  , DateTime(..)        -- re-export

  , directoryFullPath
  , streamFlat

  ) where


import qualified DirectoryMetrics.FlatSyntax as F
import DirectoryMetrics.FlatSyntax ( DateTime(..), File(..) )

import Data.List ( foldl', isPrefixOf )

data Directory = Directory
    { dir_pathto            :: String
    , dir_name              :: String
    , dir_datetime          :: DateTime
    , dir_subdirs           :: [Directory]
    , dir_files             :: [File]
    }
  deriving (Eq,Show)


directoryFullPath :: Directory -> String
directoryFullPath d = dir_pathto d ++ ('\\': dir_name d)

{-
-- | Dir listings are depth first...
--
-- Note - in the flat representation the first directory is special
-- 
streamFlat :: [F.Directory] -> [Level1]
streamFlat []        = []
streamFlat (root:subs) = aggregate (F.dirSubDirs root) subs
  where
    root_path = F.dir_pathto root

    aggregate ((F.SubDir dt nm):ks) xs = 
        let full_path   = root_path ++ ('\\':nm) 
            (kids,rest) = spans full_path xs
            files       = mapHead F.dirFiles kids
            level1      = undefined
        in level1 : aggregate ks xs

    aggregate _                 _  = []

-}


streamFlat :: [F.Directory] -> [Directory]
streamFlat = topDown


topDown :: [F.Directory] -> [Directory]
topDown = foldl' (flip insert) []


-- | /Forest/ insert
--
insert :: F.Directory -> [Directory] -> [Directory]
insert a [] = [node1 a]
insert a (d:ds) 
   | a `descendent` d = insertT a d : ds
   | otherwise        = d : insert a ds




-- | /Tree/ insert
--
insertT :: F.Directory -> Directory -> Directory
insertT a dir = case dir_subdirs dir of
   [] -> dir { dir_subdirs = [node1 a] }
   xs -> dir { dir_subdirs = insert a xs }


descendent :: F.Directory -> Directory -> Bool
descendent fd dir = (directoryFullPath dir) `isPrefixOf` (F.dir_pathto fd)


node1 :: F.Directory -> Directory
node1 dir = Directory
    { dir_pathto        = pathto
    , dir_name          = name
    , dir_datetime      = F.dirDateTime dir
    , dir_subdirs       = []
    , dir_files         = F.dirFiles dir
    }
  where
    (pathto,name) = pathSplit $ F.dir_pathto dir

spans :: String -> [F.Directory] -> ([F.Directory], [F.Directory])
spans root = span (F.dirIsChild root)



pathSplit :: String -> (String,String)
pathSplit = post . foldr fn (False,[],[]) 
  where
    fn '\\' (False,ds,ss) = (True,    ds,   ss)
    fn c    (False,ds,ss) = (False,   ds, c:ss)
    fn c    (True, ds,ss) = (True,  c:ds,   ss)

    post (_,b,c)          = (b,c)