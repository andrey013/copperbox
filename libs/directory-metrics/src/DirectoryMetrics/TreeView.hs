{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.TreeView
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


module DirectoryMetrics.TreeView 
  (
    toTree
  , toForest

  , drawDirectory
  , drawDirectories

  ) where

import DirectoryMetrics.HierSyntax

import Data.Tree

toTree :: Directory -> Tree String
toTree d = Node txt (toForest $ dir_subdirs d) 
  where
    txt = dir_name d ++ ": " ++ show (length $ dir_files d)


toForest :: [Directory] -> Forest String
toForest = map toTree 


drawDirectory :: Directory -> IO ()
drawDirectory = putStr . drawTree . toTree

drawDirectories :: [Directory] -> IO ()
drawDirectories = putStr . drawForest . toForest