{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty print note-lists using the carry symbol @(.)@ when
-- appropriate.
--
--------------------------------------------------------------------------------

module Majalan.Core.NoteList
  (

     CsEvent(..)
 
  ) where

import Majalan.Core.Basis
import Majalan.Core.Utils.DocExtras

import Control.Applicative
import qualified Data.Map as Map
import Numeric
import Text.PrettyPrint.HughesPJ


data CsEvent = CsEvent 
      { instr_num   :: Int
      , onset_time  :: Double
      , event_args  :: [Double]
      }







