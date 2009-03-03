{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.S
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- S - a set data type
--

module HMinCaml.S (
    S, 
    ofList,
    add,
    mem,
    empty,
    singleton,
    diff,
    union,
    remove
  ) where

import Data.List ( foldl' )
import qualified Data.Set as Set

type S a = Set.Set a

ofList :: Ord a => [a] -> S a
ofList = foldl' (\s e -> Set.insert e s) empty

add :: Ord a => a -> S a -> S a
add = Set.insert

mem :: Ord a => a -> S a -> Bool
mem = Set.member


empty :: S a
empty = Set.empty

singleton :: a -> S a
singleton = Set.singleton


union :: Ord a => S a -> S a -> S a
union = Set.union

diff :: Ord a => S a -> S a -> S a
diff = Set.difference

remove :: Ord a => a -> S a -> S a
remove = Set.delete
