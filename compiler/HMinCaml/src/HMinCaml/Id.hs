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

import HMinCaml.Type

type Id = String

data Label = L String
  deriving (Eq,Show)
  


idOfType :: Type -> String
idOfType TUnit        = "u"
idOfType TBool        = "b"
idOfType TInt         = "i"
idOfType TFloat       = "d"
idOfType (TFun _ _)   = "f"
idOfType (TTuple _)   = "t"
idOfType (TArray _)   = "a" 
idOfType (TVar _)     = error $ "idOfType on Var"



    