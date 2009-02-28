{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Id
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Float - not properly imlemented...
--

module HMinCaml.Float where

import Data.Int

gethi :: Float -> Int32
gethi = floor

getlo :: Float -> Int32
getlo = floor