{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcSyntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Element type for Abc
--
--------------------------------------------------------------------------------

module Mullein.AbcSyntax where

import Mullein.Pitch

import Data.Ratio

type Multiplier = Rational


data Element = Note   Pitch Multiplier
             | Rest   Multiplier
             | Spacer Multiplier
             | Chord  [Pitch] Multiplier
             | GraceNotes  [GraceNote]
  deriving (Eq,Show)

type GraceNote = Pitch


                             
