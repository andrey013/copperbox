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

  , drumLongName
  , drumShortName

  , drumScoreTrafo

  ) where


import Neume.Core.Duration
import Neume.Core.LilyPondTrafo
import Neume.Core.Syntax

import Text.PrettyPrint.Leijen



--------------------------------------------------------------------------------
-- LilyPond drum pitches 

data DrumPitch = DrumPitch 
      { drum_long_name   :: String
      , drum_short_name  :: String 
      }
  deriving (Eq,Show)


type DrumGlyph  anno dur = Glyph anno DrumPitch dur


drumLongName    :: DrumPitch -> Doc
drumLongName    = text . drum_long_name

drumShortName   :: DrumPitch -> Doc
drumShortName   = text . drum_short_name


-- lyDrumGlyph :: DrumGlyph anno (Maybe Duration) -> Doc
-- lyDrumGlyph = renderGlyph (text . drumShortName) strip



drumScoreTrafo :: LyRelDurTrafo  repr 
               => repr (DrumGlyph anno Duration) 
               -> repr (DrumGlyph anno (Maybe Duration))
drumScoreTrafo = runRelDurTrafo
               

  