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

import HMinCaml.CompilerMonad
import HMinCaml.IdTypes
import HMinCaml.Type


type Id = String

data Label = L String
  deriving (Eq,Show)
  

genid :: String -> CM String
genid s = do 
    i <- counter
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


gentmp :: Type -> CM String
gentmp typ = do 
    i <- counter
    return $ 'T' : (idOfType typ) ++ show i
    