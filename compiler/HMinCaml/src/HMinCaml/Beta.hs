{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Beta
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Beta reduction
--

module HMinCaml.Beta where

import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.KNormal

type Env = M.M Id Id

find :: Id -> Env -> Id
find x env = maybe x id (M.find x env) 

g :: Env -> Expr -> Expr
g _   Unit                = Unit
g _   (Int i)             = Int i
g _   (Float d)           = Float d
g env (Neg x)             = Neg (find x env)
g env (Add x y)           = Add (find x env) (find y env)
g env (Sub x  y)          = Sub (find x env) (find y env)
g env (FNeg x)            = FNeg (find x env)
g env (FAdd x y)          = FAdd (find x env) (find y env)
g env (FSub x y)          = FSub (find x env) (find y env)
g env (FMul x y)          = FMul (find x env) (find y env)
g env (FDiv x y)          = FDiv (find x env) (find y env)
g env (IfEq x y e1 e2)    = IfEq (find x env) (find y env) (g env e1) (g env e2)
g env (IfLE x y e1 e2)    = IfLE (find x env) (find y env) (g env e1) (g env e2)
g env (Let (x, t) e1 e2)  = case g env e1 of
                              (Var y) -> g (M.add x y env) e2
                              e1'     -> let e2' = g env e2 in
                                         Let (x, t) e1' e2'
                                         
g env (LetRec fdef e2)    = let e1 = body fdef in 
                            LetRec (fdef { body = g env e1 }) (g env e2)
     
g env (Var x)             = Var (find x env)
g env (Tuple xs)          = Tuple (map (\x -> find x env) xs)
g env (LetTuple xts y e)  = LetTuple xts (find y env) (g env e)
g env (Get x y)           = Get (find x env) (find y env)
g env (Put x y z)         = Put (find x env) (find y env) (find z env)
g env (App fn xs)         = App (find fn env) (map (\x -> find x env) xs)
g _   (ExtArray x)        = ExtArray x
g env (ExtFunApp x ys)    = ExtFunApp x (map (\y -> find y env) ys)
                                       



beta :: Expr -> Expr
beta = g M.empty
