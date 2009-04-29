{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.LilyPond
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined 
--
-- Output LilyPond
--
--------------------------------------------------------------------------------


module MulleinHaskore.LilyPond where

import MulleinHaskore.System

import Mullein.CoreTypes
import Mullein.LilyPondConvert
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.NamedElements ( c4 )
import Mullein.Pitch

import Text.PrettyPrint.Leijen


simpleLilyPond :: InstName -> Key -> MetricalSpec -> System -> Doc
simpleLilyPond name k m sys = unP $ scoreTemplate "Ly" c4 k m ly_out
  where
    ly_motif  = instMotif name k m sys
    ly_part   = linearPart ly_motif
    ly_score  = convertToLyRelative c4 ly_part
    ly_out    = generateLilyPond k (fst m) ly_score



scoreTemplate :: String 
              -> Pitch 
              -> Key 
              -> MetricalSpec 
              -> LilyPondOutput 
              -> P CtxTopLevel
scoreTemplate score_title rel_pitch key_sig meter_spec ly_output = 
    prolog +++ body 
  where
    prolog = header [title score_title]
    body   = book [score (melody rel_pitch
                                 key_sig
                                 (fst meter_spec) 
                                 (lilypondOutput ly_output))]


    




