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
import Mullein.NamedElements ( middle_c )
import Mullein.Pitch

import qualified Mullein.Score as M

import Text.PrettyPrint.Leijen

lilypond_version :: String
lilypond_version = "2.12.2"

simpleLilyPond :: InstName -> Key -> MetricalSpec -> System -> Doc
simpleLilyPond name k m sys = unP $ scoreTemplate "Ly" middle_c k m ly_out
  where
    ly_motif  = instMotif name k m sys
    ly_part   = linearPart ly_motif
    ly_score  :: M.Part 
    ly_score  = convertToLyRelative middle_c ly_part
    ly_out    = generateLilyPond k (fst m) ly_score

-- NOTE octaveDist might not be working as expected when generating ssf.lhs

scoreTemplate :: String 
              -> Pitch 
              -> Key 
              -> MetricalSpec 
              -> LilyPondOutput 
              -> P CtxTopLevel
scoreTemplate score_title rel_pitch key_sig meter_spec ly_output = 
    prolog +++ body 
  where
    prolog = version lilypond_version +++ header [title score_title]
    body   = book [score (melody rel_pitch
                                 key_sig
                                 (fst meter_spec) 
                                 (lilypondOutput ly_output))]


    




