{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.CompilerMonad
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Monad for comilation (currently just a state monad)
--

module HMinCaml.CompilerMonad where

import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.Type
import HMinCaml.Utils

import Control.Applicative
import Control.Monad.State

data CMState = CMState { id_counter :: Int, typing_extenv :: M.M Id Type }

newtype CM a = CM { getCM :: State CMState a }
  deriving ( Functor, Monad, MonadState CMState )
  
instance Applicative CM where
  pure = return
  (<*>) = ap

runCM :: CM a -> CMState -> a 
runCM m s = evalState (getCM m) s

counter :: CM Int
counter = do 
    i <- gets id_counter
    modify $ \s -> s { id_counter = i+1 }
    return i
    

gentmp :: Type -> CM String
gentmp typ = do 
    i <- counter
    return $ 'T' : (idOfType typ) ++ show i

genid :: String -> CM String
genid s = do 
    i <- counter
    return $ s ++ '.' : show i
        

    