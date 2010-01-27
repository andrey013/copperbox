{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types (e.g time signature) for musical structures.
--
--------------------------------------------------------------------------------

module M2.Datatypes
  (
    TimeSignature(..)
   
  ) where



-------------------------------------------------------------------------------
-- Time signatures

data TimeSignature = TimeSignature { ts_meter :: Int , ts_pulse :: Int }
  deriving (Eq,Show)


