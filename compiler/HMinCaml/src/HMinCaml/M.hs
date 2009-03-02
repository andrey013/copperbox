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
    empty,
    add,
    find
  ) where

import Data.List ( foldl' )
import qualified Data.Map as Map

type M k v = Map.Map k v

addList :: Ord k => [(k,v)] -> M k v -> M k v
addList xys env = foldl' (\s (k,v) -> Map.insert k v s) env xys

addList2 :: Ord k => [k] -> [v] -> M k v -> M k v
addList2 xs ys env = addList `flip` env $ zip xs ys 

empty :: M k v
empty = Map.empty

add :: Ord k => k -> v -> M k v -> M k v
add  = Map.insert

find :: Ord k => k -> M k a -> Maybe a
find = Map.lookup






