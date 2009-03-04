{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Simm13
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- 13-bit immediate optimisation
--

module HMinCaml.Simm13 where

import HMinCaml.CompilerMonad
import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.SparcAsm

import Data.Bits

g :: M.M Id Int -> SparcT -> SparcT
g env (Ans expr) = Ans(g' env expr)
g env (Let (x,t) (Set i) e) 
    | (-4096 <= i) && (i < 4096) = let e' = g (M.add x i env) e in
                                   if x `elem` (fv e') 
                                      then Let (x, t)(Set i) e' 
                                      else e'
g env (Let xt (SLL y (C i)) e)       {- for array access -}
    |  M.mem y env    = g env (Let xt (Set ((M.find' y env) `shiftL` i)) e)
g env (Let xt expr e) = Let xt (g' env expr) (g env e)
g env (Forget x e)    = Forget x (g env e)

g' :: M.M Id Int -> Expr -> Expr  
g' env (Add x (V y)) 
    | M.mem y env           = Add x (C (M.find' y env))
    | M.mem x env           = Add y (C (M.find' x env))
g' env (Sub x (V y)) 
    | M.mem y env           = Sub x (C (M.find' y env))
g' env (SLL x (V y))
    | M.mem y env           = SLL x (C (M.find' y env))
g' env (Ld x (V y))
    | M.mem y env           = Ld x (C (M.find' y env))
g' env (St x y (V z))
    | M.mem z env           = St x y (C (M.find' z env))
g' env (LdDF x (V y))
    | M.mem y env           = LdDF x (C (M.find' y env))
g' env (StDF x y (V z))
    | M.mem z env           = StDF x y (C (M.find' z env))
g' env (IfEq x (V y) e1 e2)
    | M.mem y env           = IfEq x (C (M.find' y env)) (g env e1) (g env e2)
g' env (IfLE x (V y) e1 e2)
    | M.mem y env           = IfLE x (C (M.find' y env)) (g env e1) (g env e2)
g' env (IfGE x (V y) e1 e2)
    | M.mem y env           = IfGE x (C (M.find' y env)) (g env e1) (g env e2)
g' env (IfEq x (V y) e1 e2)
    | M.mem x env           = IfEq y (C (M.find' x env)) (g env e1) (g env e2)
g' env (IfLE x (V y) e1 e2)
    | M.mem x env           = IfGE y (C (M.find' x env)) (g env e1) (g env e2)
g' env (IfGE x (V y) e1 e2)
    | M.mem x env           = IfLE y (C (M.find' x env)) (g env e1) (g env e2)
g' env (IfEq x y' e1 e2)    = IfEq x y' (g env e1) (g env e2)
g' env (IfLE x y' e1 e2)    = IfLE x y' (g env e1) (g env e2)
g' env (IfGE x y' e1 e2)    = IfGE x y' (g env e1) (g env e2)
g' env (IfFEq x y e1 e2)    = IfFEq x y (g env e1) (g env e2)
g' env (IfFLE x y e1 e2)    = IfFLE x y (g env e1) (g env e2)
g' _   e                    = e


h :: Fundef -> Fundef
h (Fundef l xs ys e t) = Fundef l xs ys (g M.empty e) t

simm13 :: Prog -> CM Prog
simm13 (Prog pdata fundefs e) = return $ 
    Prog pdata (map h fundefs) (g M.empty e)
  
  
  
        