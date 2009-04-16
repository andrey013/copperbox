{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondSyntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Element type for lilypond
--
--------------------------------------------------------------------------------

module Mullein.LilyPondSyntax where

import Mullein.Duration
import Mullein.Pitch



data Element = Note   Pitch (Maybe Duration)
             | Rest   (Maybe Duration)
             | Spacer (Maybe Duration)
             | Chord  [Pitch] (Maybe Duration)
             | GraceNotes  [GraceNote]
  deriving (Eq,Show)

type GraceNote = Pitch


                             
