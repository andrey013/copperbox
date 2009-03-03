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
import qualified  HMinCaml.S as S
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

fv :: Expr -> S.S Id
fv Unit               = S.empty
fv (Int _)            = S.empty 
fv (Float _)          = S.empty 
fv (ExtArray _)       = S.empty
fv (Neg x)            = S.singleton x
fv (FNeg x)           = S.singleton x
fv (Add x y)          = S.ofList [x,y]
fv (Sub x y)          = S.ofList [x,y]
fv (FAdd x y)         = S.ofList [x,y]
fv (FSub x y)         = S.ofList [x,y]
fv (FMul x y)         = S.ofList [x,y]
fv (FDiv x y)         = S.ofList [x,y]
fv (Get x y)          = S.ofList [x,y]
fv (IfEq x y e1 e2)   = S.add x $ S.add y (S.union (fv e1) (fv e2)) 
fv (IfLE x y e1 e2)   = S.add x $ S.add y (S.union (fv e1) (fv e2))
fv (Let (x,_) e1 e2)  = S.union (fv e1) (S.remove x (fv e2))
fv (Var x)            = S.singleton x
fv (LetRec (Fundef (x,_) yts e1) e2) =
      let zs = S.diff (fv e1) (S.ofList (map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
      
fv (App x ys)         = S.ofList (x:ys)
fv (Tuple xs)         = S.ofList xs 
fv (ExtFunApp _ xs)   = S.ofList xs
fv (Put x y z)        = S.ofList [x,y,z]
fv (LetTuple xs y e)  = S.add y (S.diff (fv e) (S.ofList (map fst xs)))
  
  
  