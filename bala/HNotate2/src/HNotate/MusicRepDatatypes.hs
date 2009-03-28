{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MusicRepDatatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Further data for representing music (other than pitch and duration) 
-- which have their own modules
--
--------------------------------------------------------------------------------

module HNotate.MusicRepDatatypes where


-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Int Int 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)
  