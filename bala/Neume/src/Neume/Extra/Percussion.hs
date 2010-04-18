{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.Percussion
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Percussion notation for LilyPond
--
--------------------------------------------------------------------------------

module Neume.Extra.Percussion
  (

  
  -- * LilyPond drum pitches
    DrumPitch(..)
  , DrumGlyph
  , DrumGlyph'
  , lyDrumGlyph

  , Ly_drums_rewrite_config(..)
  , renderLyDrums

  ) where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxInterim
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils

import Text.PrettyPrint.Leijen



--------------------------------------------------------------------------------
-- LilyPond drum pitches 

data DrumPitch = DrumPitch { 
      drumLongName   :: String, 
      drumShortName  :: String 
    }
  deriving (Eq,Show)


type DrumGlyph  anno = Glyph anno DrumPitch Duration
type DrumGlyph' anno = Glyph anno DrumPitch (Maybe Duration) 


lyDrumGlyph :: DrumGlyph' anno -> Doc
lyDrumGlyph = renderGlyph (text . drumShortName) strip




--------------------------------------------------------------------------------


data Ly_drums_rewrite_config anno = Ly_drums_rewrite_config
    { meter_pattern_drums       :: MeterPattern 
    , anno_printer_drums        :: anno -> DocS
    } 



renderLyDrums :: Ly_std_format_config
              -> Ly_drums_rewrite_config anno
              -> Score sh (NoteList (DrumGlyph anno))
              -> Doc
renderLyDrums (Ly_std_format_config func) (Ly_drums_rewrite_config mp annof) = 
    concatDocSections func . renderScoreDrums mp annof


renderScoreDrums :: MeterPattern 
                 -> (anno -> DocS)
                 -> Score sh (NoteList (DrumGlyph anno))
                 -> Score sh PhraseImage
renderScoreDrums mp annof = fmap (phraseImageDrums mp annof) 


phraseImageDrums :: MeterPattern
                 -> (anno -> DocS)
                 -> NoteList (DrumGlyph anno)
                 -> PhraseImage
phraseImageDrums mp annof = 
    renderPhrase renderf . rewriteDurationOpt . phrase mp
  where
    renderf = renderGlyph (text . drumShortName) annof

