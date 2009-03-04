{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.IdTypes
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Just the Id types (to avoid cyclic dependencies)
--

module HMinCaml.IdTypes ( Id, Label (..) ) where


type Id = String

data Label = L String
  deriving (Eq,Show)
  


    