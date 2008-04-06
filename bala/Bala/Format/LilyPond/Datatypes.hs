
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for a subset of LilyPond format
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Datatypes  where


data Rest = Rest Int


data Pitch = Pitch Char (Maybe OctaveSpec)
  deriving (Eq, Show)

data OctaveSpec = Raised Int | Lowered Int
  deriving (Eq, Show)
  
data Accidental = Sharp | Flat | DoubleSharp | DoubleFlat 
  deriving (Eq, Show)
  
data MicroTone =  HalfFlat | HalfSharp 
  deriving (Eq, Show) 