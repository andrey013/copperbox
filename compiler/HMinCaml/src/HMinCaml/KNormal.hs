{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.KNormal
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- K Normal form datatypes
--

module HMinCaml.KNormal where

import HMinCaml.Id
import HMinCaml.Type

data Expr = Unit
          | Int       Int
          | Float     Float
          | Neg       Id
          | Add       Id    Id
          | Sub       Id    Id
          | FNeg      Id
          | FAdd      Id    Id
          | FSub      Id    Id
          | FMul      Id    Id
          | FDiv      Id    Id
          | IfEq      Id    Id    Expr  Expr
          | IfLE      Id    Id    Expr  Expr
          | Let       (Id, Type)  Expr  Expr
          | Var       Id
          | LetRec    Fundef  Expr
          | App       Id    [Id]
          | Tuple     [Id]
          | LetTuple  [(Id, Type)]  Id  Expr
          | Get       Id    Id
          | Put       Id    Id    Id
          | ExtArray  Id
          | ExtFunApp Id    [Id]
  deriving (Eq,Show)
  
data Fundef = Fundef
      { name :: (Id, Type)
      , args :: [(Id, Type)]
      , body :: Expr 
      }
  deriving (Eq,Show)

  
  