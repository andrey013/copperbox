{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Typing
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Type inference
--

module HMinCaml.Typing  where

import HMinCaml.CompilerMonad
import HMinCaml.Id
import HMinCaml.M
import HMinCaml.Syntax
import HMinCaml.Type

import Control.Applicative hiding ( empty )
  


derefTyp :: Type -> CM Type
derefTyp (TFun t1s t2)    = TFun    <$> (mapM derefTyp t1s) <*> derefTyp t2
derefTyp (TTuple ts)      = TTuple  <$> mapM derefTyp ts
derefTyp (TArray t)       = TArray  <$> derefTyp t
derefTyp (TVar Nothing)   = return $ TVar (Just TInt)
derefTyp (TVar (Just _t)) = error $ "do TVars need STRefs?"

derefTyp t                = return t
  
derefIdTyp :: (Id,Type) -> CM (Id,Type)
           
derefIdTyp (x, t) = (,) <$> pure x <*> derefTyp t

derefTerm :: Expr -> CM Expr
derefTerm (Not e) =  Not <$> derefTerm e

derefTerm _       = error $ "derefTerm todo"

typing :: Expr -> CM Expr
typing e = do
    -- ...
    derefTerm e
    
         

    