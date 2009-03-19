{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Elim
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Eliminate unnecessary definitions
--

module HMinCaml.Elim where

import HMinCaml.CompilerMonad
import HMinCaml.KNormal
import HMinCaml.KNormalSyn
import qualified HMinCaml.S as S
import HMinCaml.ToKNormal

import Data.List ( find )


effect :: Expr -> Bool
effect (Let _ e1 e2)      = effect e1 || effect e2
effect (IfEq _ _ e1 e2)   = effect e1 || effect e2
effect (IfLE _ _ e1 e2)   = effect e1 || effect e2
effect (LetRec _ e)       = effect e 
effect (LetTuple _ _ e)   = effect e
effect (App _ _)          = True
effect (Put _ _ _)        = True
effect (ExtFunApp _ _)    = True
effect _                  = False

f :: Expr -> Expr         
f (IfEq x y e1 e2)    = IfEq x y (f e1) (f e2)
f (IfLE x y e1 e2)    = IfLE x y (f e1) (f e2)
f (Let (x,t) e1 e2)   = let e1' = f e1; e2' = f e2 in
    if effect e1' || S.mem x (fv e2') then Let (x,t) e1' e2' else e2'
    
f (LetRec (Fundef (x,t) yts e1) e2) = let e2' = f e2 in
    if S.mem x (fv e2') then LetRec (Fundef (x,t) yts (f e1)) e2' else e2'

f (LetTuple xts y e)  = let xs    = map fst xts
                            e'    = f e 
                            live  = fv e' 
                        in maybe e' (const $ LetTuple xts y e')
                                    (find (\x -> S.mem x live) xs)
                                                                    
f e                    = e

elim :: Expr -> CM Expr 
elim = return . f
