{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Id
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Identifiers
--

module HMinCaml.Id where


type Id = String

data Label = L String
  deriving (Eq,Show)

newId :: String -> Int -> Id
newId s x = s ++ show x
  
{-



-}    