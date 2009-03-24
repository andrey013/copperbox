{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.M
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- M - customized Data.Map
--

module HMinCaml.M (
    M,
    addList,
    addList2,
    mem,
    empty,
    add, add2,
    find, find', map
  ) where

import HMinCaml.Utils ( foldleft2 )

import Data.List ( foldl' )
import qualified Data.Map as Map

import Prelude hiding ( map )

type M k v = Map.Map k v




addList :: Ord k => [(k,v)] -> M k v -> M k v
addList xys env = foldl' (\s (k,v) -> Map.insert k v s) env xys

addList2 :: Ord k => [k] -> [v] -> M k v -> M k v
addList2 xs ys env = foldleft2 (\s k v -> Map.insert k v s) env xs ys 

empty :: M k v
empty = Map.empty

add :: Ord k => k -> v -> M k v -> M k v
add  = Map.insert

add2 :: Ord k => (k,v) -> M k v -> M k v
add2 (k,v) = Map.insert k v


find :: Ord k => k -> M k a -> Maybe a
find = Map.lookup
    
    
find' :: Ord k => k -> M k a -> a
find' k m = maybe fk id $ Map.lookup k m where
    fk = error $ "error missing M.find" 
              


mem :: Ord k => k -> M k a -> Bool
mem = Map.member

map :: (a -> b) -> M k a -> M k b
map = Map.map
