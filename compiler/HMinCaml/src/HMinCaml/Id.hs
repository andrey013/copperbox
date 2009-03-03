{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Id
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Identifiers
--

module HMinCaml.Id where

import HMinCaml.Type

import Control.Monad.State

type Id = String

data Label = L String
  deriving (Eq,Show)
  

genid :: MonadState Int m => String -> m String
genid s = do 
    i <- get
    put (i+1)
    return $ s ++ '.' : show i
    
idOfType :: Type -> String
idOfType TUnit        = "u"
idOfType TBool        = "b"
idOfType TInt         = "i"
idOfType TFloat       = "d"
idOfType (TFun _ _)   = "f"
idOfType (TTuple _)   = "t"
idOfType (TArray _)   = "a" 
idOfType (TVar _)     = error $ "idOfType on Var"


gentmp :: MonadState Int m => Type -> m String
gentmp typ = do 
    i <- get
    put (i+1)
    return $ 'T' : (idOfType typ) ++ show i
    