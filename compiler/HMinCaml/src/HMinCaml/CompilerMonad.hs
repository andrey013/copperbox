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



import Control.Applicative
import Control.Monad.State

instance Applicative (State Int) where
  pure = return
  (<*>) = ap

eval = evalState