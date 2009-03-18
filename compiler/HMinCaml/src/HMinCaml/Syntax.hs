

-- UUAGC 0.9.6 (Syntax.ag)


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


gentyp :: Type
gentyp = TVar Nothing

-- Expr --------------------------------------------------------
data Expr  = Add (Expr) (Expr) 
           | App (Expr) (Exprs) 
           | Array (Expr) (Expr) 
           | Bool (Bool) 
           | Eq (Expr) (Expr) 
           | FAdd (Expr) (Expr) 
           | FDiv (Expr) (Expr) 
           | FMul (Expr) (Expr) 
           | FNeg (Expr) 
           | FSub (Expr) (Expr) 
           | Float (Float) 
           | Get (Expr) (Expr) 
           | If (Expr) (Expr) (Expr) 
           | Int (Int) 
           | LE (Expr) (Expr) 
           | Let (TypeId) (Expr) (Expr) 
           | LetRec (Fundef) (Expr) 
           | LetTuple (TypeIds) (Expr) (Expr) 
           | Neg (Expr) 
           | Not (Expr) 
           | Put (Expr) (Expr) (Expr) 
           | Sub (Expr) (Expr) 
           | Tuple (Exprs) 
           | Unit 
           | Var (Id) 
           deriving ( Eq,Show)
-- Exprs -------------------------------------------------------
type Exprs  = [Expr]
-- Fundef ------------------------------------------------------
data Fundef  = Fundef (TypeId) (TypeIds) (Expr) 
             deriving ( Eq,Show)