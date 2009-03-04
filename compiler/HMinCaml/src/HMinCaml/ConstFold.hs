{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.ConstFold
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Constant folding
--

module HMinCaml.ConstFold where

import HMinCaml.CompilerMonad
import HMinCaml.Id
import HMinCaml.KNormal ( Expr(..), Fundef(..) )
import qualified HMinCaml.M as M
import HMinCaml.Utils

type Env = M.M Id Expr

memi :: Id -> Env -> Bool


memi x env = case M.find x env of
                Just (Int _) -> True
                _            -> False
memf :: Id -> Env -> Bool          
memf x env = case M.find x env of
                Just (Float _) -> True
                _              -> False

memt :: Id -> Env -> Bool               
memt x env = case M.find x env of
                Just (Tuple _) -> True
                _              -> False  

findi :: Id -> Env -> Int   
findi x env = case M.find x env of
                Just (Int i) -> i 
                _            -> error "ConstFold findi - Not_found"

findf :: Id -> Env -> Float                 
findf x env = case M.find x env of
                Just (Float d) -> d 
                _              -> error "ConstFold findf - Not_found"

findt :: Id -> Env -> [Id]                 
findt x env = case M.find x env of
                Just (Tuple ys) -> ys
                _               -> error "ConstFold findt - Not_found"


g :: Env -> Expr -> Expr
g env (Var x) | memi x env      = Int (findi x env)           
g env (Neg x) | memi x env      = Int (negate (findi x env))
g env (Add x y) 
    | memi x env && memi y env  = Int (findi x env + findi y env)
g env (Sub x y) 
    | memi x env && memi y env  = Int (findi x env - findi y env)
g env (FNeg x) 
    | memf x env                = Float (negate (findf x env))
g env (FAdd x y) 
    | memf x env && memf y env  = Float (findf x env + findf y env)
g env (FSub x y)
    | memf x env && memf y env  = Float (findf x env - findf y env)
g env (FMul x y) 
    | memf x env && memf y env  = Float (findf x env * findf y env)
g env (FDiv x y) 
    | memf x env && memf y env  = Float (findf x env / findf y env)
g env (IfEq x y e1 e2) 
    | memi x env && memi y env  = if findi x env == findi y env 
                                    then g env e1 
                                    else g env e2
                                    
g env (IfEq x y e1 e2) 
    | memf x env && memf y env  = if findf x env == findf y env 
                                    then g env e1 
                                    else g env e2
                                    
g env (IfEq x y e1 e2)          = IfEq x y (g env e1) (g env e2)
g env (IfLE x y e1 e2) 
    | memi x env && memi y env  = if findi x env <= findi y env 
                                    then g env e1 
                                    else g env e2
                                    
g env (IfLE x y e1 e2)
    | memf x env && memf y env  = if findf x env <= findf y env 
                                    then g env e1 
                                    else g env e2
                                    
g env (IfLE x y e1 e2)          = IfLE x y (g env e1) (g env e2)
g env (Let (x,t) e1 e2)         = let e1' = g env e1
                                      e2' = g (M.add x e1' env) e2 
                                  in Let (x,t) e1' e2'
                                  
g env (LetRec (Fundef x ys e1) e2)  = LetRec (Fundef x ys (g env e1)) (g env e2)
g env (LetTuple xts y e) 
    | memt y env                = foldleft2 (\e' xt z -> Let xt (Var z) e')
                                            (g env e)
                                            xts
                                            (findt y env)
    | otherwise                 = LetTuple xts y (g env e)
  
g _   e                         = e


                
constFold :: Expr -> CM Expr
constFold = return . g M.empty