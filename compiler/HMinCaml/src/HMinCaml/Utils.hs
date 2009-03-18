{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Utils
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- S - a set data type
--

module HMinCaml.Utils  where

import HMinCaml.Type

foldleft2           :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
foldleft2 f z0 xs0 ys0 = lgo z0 xs0 ys0
    where lgo z []     _      = z
          lgo z _      []     = z
          lgo z (x:xs) (y:ys) = let z' = f z x y in z' `seq` lgo z' xs ys

          

    
idOfType :: Type -> String
idOfType TUnit        = "u"
idOfType TBool        = "b"
idOfType TInt         = "i"
idOfType TFloat       = "d"
idOfType (TFun _ _)   = "f"
idOfType (TTuple _)   = "t"
idOfType (TArray _)   = "a" 
idOfType (TVar _)     = error $ "idOfType on Var"


    