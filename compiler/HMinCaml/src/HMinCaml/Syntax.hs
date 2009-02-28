{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Syntax
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Syntax
--

module HMinCaml.Syntax where

import HMinCaml.Id
import HMinCaml.Type

data Expr = Unit
          | Bool      Bool
          | Int       Int
          | Float     Float
          | Not       Expr
          | Neg       Expr
          | Add       Expr  Expr
          | Sub       Expr  Expr
          | FNeg      Expr
          | FAdd      Expr  Expr
          | FSub      Expr  Expr
          | FMul      Expr  Expr
          | FDiv      Expr  Expr
          | Eq        Expr  Expr
          | LE        Expr  Expr
          | If        Expr  Expr  Expr
          | Let       (Id, Type)  Expr  Expr
          | Var       Id
          | LetRec    Fundef  Expr
          | App       Expr            [Expr]
          | Tuple     [Expr]
          | LetTuple  [(Id, Type)]  Expr  Expr
          | Array     Expr  Expr
          | Get       Expr  Expr
          | Put       Expr  Expr  Expr
  deriving (Eq,Show)
  
data Fundef = Fundef 
      { name :: (Id, Type)
      , args :: [(Id, Type)]
      , body :: Expr 
      }
  deriving (Eq,Show)
  
                

