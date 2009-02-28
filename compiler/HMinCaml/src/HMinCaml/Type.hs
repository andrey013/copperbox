{-# OPTIONS -Wall #-}

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


data Type = TUnit
          | TBool
          | TInt
          | TFloat
          | TFun   [Type]  Type         {- arguments are uncurried -}
          | TTuple [Type]
          | TArray Type
          | TVar   (Maybe Type)
  deriving (Eq,Show)

gentyp :: Type
gentyp = TVar Nothing
  
         
  