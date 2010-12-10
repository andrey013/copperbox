{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.BoxNotation
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- /Tagless/-style type classes representing the sets of Djembe 
-- and other intrument \"notes\" in the book 
-- /West Affrican Percussions/ by Paul Nas and others.
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.BoxNotation
  (
    CBoxDjembe(..)
  , CBoxKenkeni(..)
  , CBoxSangban(..)

  ) where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.GraphicInterpretation
import Wumpus.Rhythm.Djembe.GraphicPrimitives 



class (CStrokeBase repr, CStrokeAnno repr) => CBoxDjembe repr where
  bass             :: repr
  muffled_bass     :: repr
  tone             :: repr
  muffled_tone     :: repr
  slap             :: repr
  muffled_slap     :: repr
  bass_flam        :: repr
  tone_flam        :: repr
  slap_flam        :: repr


class (CStrokeBase repr, CStrokeAnno repr) => CBoxKenkeni repr where
  kenkeni_stroke            :: repr
  kenkeni_pressed_stroke    :: repr


class (CStrokeBase repr, CStrokeAnno repr) => CBoxSangban repr where
  sangban_stroke            :: repr
  sangban_pressed_stroke    :: repr


--------------------------------------------------------------------------------
-- G instances
  
instance CBoxDjembe G where
  bass              = G $ djembeNote $ letterNotehead 667 'B'
  muffled_bass      = G $ djembeNote $ addAngledStrike $ letterNotehead 667 'B'
  tone              = G $ djembeNote $ dotNotehead
  muffled_tone      = G $ djembeNote $ addAngledStrike $ dotNotehead
  slap              = G $ djembeNote $ letterNotehead 667 'X'
  muffled_slap      = G $ djembeNote $ addBaselineStrike $ letterNotehead 667 'X'
  bass_flam         = G $ flamNote (letterNotehead 667 'B')
                                   (letterFlamGlyph 667 'B')
  tone_flam         = G $ flamNote dotNotehead dotFlamGlyph
  slap_flam         = G $ flamNote (letterNotehead 667 'X')
                                   (letterFlamGlyph 667 'X')


instance CBoxKenkeni G where
  kenkeni_stroke            = G $ djembeNote $ dotNotehead
  kenkeni_pressed_stroke    = G $ djembeNote $ addAngledStrike $ dotNotehead


instance CBoxSangban G where
  sangban_stroke            = G $ djembeNote $ dotNotehead
  sangban_pressed_stroke    = G $ djembeNote $ addAngledStrike $ dotNotehead
