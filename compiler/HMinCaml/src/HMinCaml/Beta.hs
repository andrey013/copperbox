{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Beta
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Beta reduction
--

module HMinCaml.Beta where

import HMinCaml.M
import HMinCaml.Syntax

-- urk - looks like the env is a map of Map Expr Expr...

betaG :: M a a -> Expr -> Expr
betaG _   Unit        = Unit
betaG _   (Int i)     = Int i
betaG _   (Float d)   = Float d

betaG _   _           = error $ "betaG"


beta :: Expr -> Expr



beta = betaG empty