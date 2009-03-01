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

import HMinCaml.Id
import HMinCaml.M
import HMinCaml.Syntax
import HMinCaml.Type

import Control.Applicative hiding ( empty )
import Control.Monad.State

data TypingState = TypingState { extenv :: M Id Type } 


instance Applicative (State TypingState) where
  pure = return
  (<*>) = ap
  
  
initial_state :: TypingState
initial_state = TypingState empty

derefTyp :: (Applicative m , MonadState TypingState m) => Type -> m Type
derefTyp (TFun t1s t2)    = TFun    <$> (mapM derefTyp t1s) <*> derefTyp t2
derefTyp (TTuple ts)      = TTuple  <$> mapM derefTyp ts
derefTyp (TArray t)       = TArray  <$> derefTyp t
derefTyp (TVar Nothing)   = return $ TVar (Just TInt)
derefTyp (TVar (Just _t)) = error $ "do TVars need STRefs?"

derefTyp t                = return t
  
derefIdTyp :: (Applicative m , MonadState TypingState m) 
           => (Id,Type) -> m (Id,Type)
           
derefIdTyp (x, t) = (,) <$> pure x <*> derefTyp t

derefTerm :: (Applicative m , MonadState TypingState m) 
          => Expr -> m Expr
derefTerm (Not e) =  Not <$> derefTerm e

derefTerm _       = error $ "derefTerm todo"

typing :: Expr -> Expr
typing e = evalState `flip` initial_state $ do
    -- ...
    derefTerm e
    
         

    