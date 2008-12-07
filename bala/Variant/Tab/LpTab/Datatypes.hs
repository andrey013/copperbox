
--------------------------------------------------------------------------------
-- |
-- Module      :  LpTab.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Data types
--
--------------------------------------------------------------------------------



module LpTab.Datatypes where

import Bala.Base
import Data.Ratio


  
data TabLexeme = BarMarkT             -- '|'
               | HiatonT              -- '-'
               | FretNumberT Int      -- 0..
  deriving (Eq,Ord,Show)                 

type TabLine    = [TabLexeme]

data TabSystem  = TabSystem [TabLine]
  deriving (Show)

type Onset = Ratio Int

type OnsetEvent evt = (Onset, evt)

type StringNumber = Int
type Fret  = Int
data MonoEvent = NoteME StringNumber Fret
               | ChordME [(StringNumber, Fret)]
               | NoME               
  deriving (Eq,Ord,Show)         
   

newtype BarF a = Bar { getBar :: [a] }
  deriving (Show)

instance Functor BarF where
  fmap f (Bar xs) = Bar (fmap f xs)
  
type Tuning = [Pitch]



data LyElementF dur = LyNote Pitch StringNumber dur
                    | LyChord [(Pitch,StringNumber)] dur
                    | LyRest dur
  deriving (Eq,Show)                    
 
  