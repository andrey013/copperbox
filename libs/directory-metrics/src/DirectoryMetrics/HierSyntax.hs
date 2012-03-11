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
    Level1(..)

  , File(..)            -- re-export
  , SubDirectory(..)

  -- * Re-export
  , DateTime(..)

  , streamFlat

  ) where


import qualified DirectoryMetrics.FlatSyntax as F
import DirectoryMetrics.FlatSyntax ( DateTime(..), File(..) )

import Data.List

data Level1 = Level1
    { lvl_one_pathto            :: String
    , lvl_one_name              :: String
    , lvl_one_datetime          :: DateTime
    , lvl_one_subdirs           :: [SubDirectory]
    , lvl_one_files             :: [File]
    }
  deriving (Eq,Show)


data SubDirectory = SubDir 
      { sub_dir_name            :: String
      , sub_dir_datetime        :: DateTime
      , sub_dir_children        :: [SubDirectory]
      , sub_dir_files           :: [File]
      }
  deriving (Eq,Show)



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


spans :: String -> [F.Directory] -> ([F.Directory], [F.Directory])
spans root = span (F.dirIsChild root)

mapHead :: (a -> [b]) -> [a] -> [b]
mapHead f []    = []
mapHead f (a:_) = f a

descend1 :: String -> [F.Directory] -> ([SubDirectory], [F.Directory])
descend1 _         []         = ([],[])
descend1 root_path subs@(s:_)
    | not (root_path `isPrefixOf` F.dir_pathto s) = ([],subs)
    | otherwise  = undefined
