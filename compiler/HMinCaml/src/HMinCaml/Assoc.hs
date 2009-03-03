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


import HMinCaml.KNormal ( Expr(..), Fundef(..) )

assoc :: Expr -> Expr
assoc (IfEq x y e1 e2)      = IfEq x y (assoc e1) (assoc e2)
assoc (IfLE x y e1 e2)      = IfLE x y (assoc e1) (assoc e2)
assoc (Let xt e1 e2)        = insert (assoc e1) where
    insert (Let yt e3 e4)     = Let yt e3 (insert e4)
    insert (LetRec fundefs e) = LetRec fundefs (insert e)
    insert (LetTuple yts z e) = LetTuple yts z (insert e)
    insert e                  = Let xt e (assoc e2)
              
assoc (LetRec (Fundef xt yts e1) e2)  = LetRec (Fundef xt yts (assoc e1)) (assoc e2)
assoc (LetTuple xts y e)              = LetTuple xts y (assoc e)
assoc e                               = e
