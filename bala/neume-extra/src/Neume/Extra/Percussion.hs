{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
    DrumName
  , DrumPitch(..)
  , DrumGlyph

  , drumAlg 

  , drumLongName
  , drumShortName


  ) where


import Neume.Core.Duration
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondTrafo
import Neume.Core.Syntax

import Neume.Extra.Common
import Neume.Extra.LilyPondScoreOutput

import Text.PrettyPrint.Leijen          -- package: wl-pprint



--------------------------------------------------------------------------------
-- LilyPond drum pitches 

type DrumName = String


data DrumPitch = DrumPitch 
      { drum_long_name   :: String
      , drum_short_name  :: String 
      }
  deriving (Eq,Show)


type DrumGlyph  anno dur = Glyph anno DrumPitch dur



drumAlg :: (LyRelDurTrafo repr)
        => (DrumPitch -> Doc) 
        -> LilyPondImageAlg repr (DrumGlyph anno Duration)
                                 (DrumGlyph anno (Maybe Duration))
drumAlg pp = LilyPondImageAlg
    { glyph_printer     = renderGlyph pp strip
    , duration_trafo    = fmap runRelDurTrafo
    , pitch_trafo       = id
    }




drumLongName    :: DrumPitch -> Doc
drumLongName    = text . drum_long_name

drumShortName   :: DrumPitch -> Doc
drumShortName   = text . drum_short_name




  