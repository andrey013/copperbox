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
import HNotate.NoteListDatatypes

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



data ScoreProcessor m template target_fragment target = ScoreProcessor {
    
    -- | Backend specific rewrite step.
    reformulate_notelist      :: OutputScheme -> NoteList -> NotateT m NoteList,
    
    -- | Convert a notelist into a target. 
    --   This isn't final output, the target framents will need assembly.
    notelist_to_target        :: NoteList -> NotateT m target_fragment,
    
    
    assemble_target_fragments :: template -> [target_fragment] -> NotateT m target,
    
    -- Finally write the output to file.    
    output_to_file            :: FilePath -> target -> IO ()
  }
 
    

