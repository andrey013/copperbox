{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Assoc
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Flatten let-bindings for pretty printing
--

module HMinCaml.Assoc where

import HMinCaml.CompilerMonad
import HMinCaml.KNormal ( Expr(..), Fundef(..) )

import Control.Monad ( liftM )

g :: Expr -> Expr
g (IfEq x y e1 e2)                = IfEq x y (g e1) (g e2)
g (IfLE x y e1 e2)                = IfLE x y (g e1) (g e2)
g (Let xt e1 e2)                  = insert (g e1) where
    insert (Let yt e3 e4)     = Let yt e3 (insert e4)
    insert (LetRec fundefs e) = LetRec fundefs (insert e)
    insert (LetTuple yts z e) = LetTuple yts z (insert e)
    insert e                  = Let xt e (g e2)
              
g (LetRec (Fundef xt yts e1) e2)  = LetRec (Fundef xt yts (g e1)) (g e2)
g (LetTuple xts y e)              = LetTuple xts y (g e)
g e                               = e

assoc :: Expr -> CM Expr
assoc = return . g

