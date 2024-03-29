{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ProcessingBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Types for pre- and post- processing NoteLists
--
--------------------------------------------------------------------------------

module HNotate.ProcessingBase where

import HNotate.Document
import HNotate.Env
import HNotate.NotateMonad

import Control.Applicative
import Control.Monad

data OutputScheme = OutputRelative | OutputDefault
  deriving (Eq,Show)
  
  
-- Type specialization now that we have the Env and Config
type NotateT m a = NotateMonadT Env Config m a

runNotateT :: Monad m => 
                NotateT m a -> Env -> Config -> m (Either NotateErr a,String)
runNotateT = runNotateMonadT

instance (Monad m ) => Applicative (NotateMonadT Env Config m) where
  pure = return
  (<*>) = ap
  
  
-- Should each note list have a bcf or can the env have a single one?
type BarConcatFun = [(Int,ODoc)] -> ODoc


    

