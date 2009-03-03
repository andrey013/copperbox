{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Inline
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Inline expansion
--

module HMinCaml.Inline where

import qualified HMinCaml.Alpha as Alpha
import HMinCaml.KNormal ( Expr(..), Fundef(..) )
import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.Type
import HMinCaml.Utils ( foldleft2 )

import Control.Monad.State

type Env = M.M Id ([(Id, Type)], Expr)

-- this is configurable in MinCaml
threshold :: Int 
threshold = 100
  
size :: Expr -> Int
size (IfEq _ _ e1 e2)   = 1 + size e1 + size e2
size (IfLE _ _ e1 e2)   = 1 + size e1 + size e2
size (Let _ e1 e2)      = 1 + size e1 + size e2
size (LetRec fdef e2)   = 1 + size (body fdef) + size e2
size (LetTuple _ _ e)   = 1 + size e
size _                  = 1


g :: Env -> Expr -> Expr
g env (IfEq x y e1 e2)    = IfEq x y (g env e1) (g env e2)
g env (IfLE x y e1 e2)    = IfLE x y (g env e1) (g env e2)
g env (Let xt e1 e2)      = Let xt (g env e1) (g env e2)
g env (LetRec (Fundef (x,t) yts e1) e2) =
      let env' = if size e1 > threshold then env else M.add x (yts, e1) env in
      LetRec (Fundef (x,t) yts (g env' e1)) (g env' e2)

g env (App x ys) 
    | M.mem x env         = let (zs,e) = M.find' x env
                                env'   = foldleft2
                                          (\env0 (z,_) y -> M.add z y env0)
                                          M.empty
                                          zs
                                          ys
                            in evalState (Alpha.g env' e) 0 --- whoops 
g env (LetTuple xts y e)  = LetTuple xts y (g env e)
g _   e                   = e
inline :: Expr -> Expr 
inline = g M.empty  



  