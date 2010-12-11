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
  , CBoxDoundounba(..)
  , CBoxDundun(..)

  ) where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.GraphicInterpretation
import Wumpus.Rhythm.Djembe.GraphicPrimitives 

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core

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

class (CStrokeBase repr, CStrokeAnno repr) => CBoxDoundounba repr where
  doundounba_stroke            :: repr
  doundounba_pressed_stroke    :: repr


-- Tthe tagless style is handy as we can use inheritance here...

class ( CStrokeBase repr, CStrokeAnno repr
      , CBoxSangban repr, CBoxDoundounba repr ) => CBoxDundun repr where
  dundun_slap_bass              :: repr


--------------------------------------------------------------------------------
-- GDjembe instances

letter :: Char -> DLocImage AfmUnit
letter = letterNotehead . CharLiteral

upstroke :: Char -> DLocGraphic
upstroke = upstrokeLetter . CharLiteral

letterFlam :: Char -> DLocGraphic 
letterFlam = letterFlamGlyph . CharLiteral
  
instance CBoxDjembe GDjembe where
  bass          = GDjembe $ djembeNote $ letter 'B'
  muffled_bass  = GDjembe $ djembeNote $ addAngledStrike $ letter 'B'
  tone          = GDjembe $ djembeNote $ dotNotehead
  muffled_tone  = GDjembe $ djembeNote $ addAngledStrike $ dotNotehead
  slap          = GDjembe $ djembeNote $ letter 'X'
  muffled_slap  = GDjembe $ djembeNote $ addBaselineStrike $ letter 'X'
  bass_flam     = GDjembe $ flamNote (letter 'B') (letterFlam 'B')
  tone_flam     = GDjembe $ flamNote dotNotehead dotFlamGlyph
  slap_flam     = GDjembe $ flamNote (letter 'X') (letterFlam 'X')


instance CBoxKenkeni GDjembe where
  kenkeni_stroke            = GDjembe $ djembeNote $ dotNotehead
  kenkeni_pressed_stroke    = GDjembe $ djembeNote $ addAngledStrike $ dotNotehead


instance CBoxSangban GDjembe where
  sangban_stroke            = GDjembe $ djembeNote $ dotNotehead
  sangban_pressed_stroke    = GDjembe $ djembeNote $ addAngledStrike $ dotNotehead


instance CBoxDoundounba GDjembe where
  doundounba_stroke         = GDjembe $ djembeNote $ letter 'B'
  doundounba_pressed_stroke = GDjembe $ djembeNote $ addAngledStrike 
                                                   $ letter 'B'



instance CBoxDundun GDjembe where
  dundun_slap_bass         = GDjembe $ djembeNote ddsb


ddsb :: DLocImage AfmUnit
ddsb = superimposeLocImage (letter 'B') (upstroke 'X')
