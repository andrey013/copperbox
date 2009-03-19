

-- UUAGC 0.9.6 (Type.ag)


-- |
-- Module: HMinCaml.Type
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Type representation
--


module HMinCaml.Type where

import HMinCaml.Id

-- LabeledType -------------------------------------------------
type LabeledType  = ( Label,Type)
-- OptType -----------------------------------------------------
type OptType  = (Maybe (Type))
-- Type --------------------------------------------------------
data Type  = TArray (Type) 
           | TBool 
           | TFloat 
           | TFun (Types) (Type) 
           | TInt 
           | TTuple (Types) 
           | TUnit 
           | TVar (OptType) 
           deriving ( Eq,Show)
-- TypeId ------------------------------------------------------
type TypeId  = ( Id,Type)
-- TypeIds -----------------------------------------------------
type TypeIds  = [TypeId]
-- Types -------------------------------------------------------
type Types  = [Type]