{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Alpha
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Alpha conversion
--

module HMinCaml.Alpha where

import HMinCaml.M
import HMinCaml.Syntax

-- urk - looks like the env is a map of Map Expr Expr...

alphaG :: M a a -> Expr -> Expr
alphaG _   Unit        = Unit
alphaG _   (Int i)     = Int i
alphaG _   (Float d)   = Float d

alphaG _   _           = error $ "betaG"


alpha :: Expr -> Expr
alpha = betaG empty