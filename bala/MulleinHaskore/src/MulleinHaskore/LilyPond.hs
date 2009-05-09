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

scoreTemplate :: String 
              -> Pitch 
              -> Key 
              -> MetricalSpec 
              -> LilyPondOutput 
              -> P CtxTopLevel
scoreTemplate score_title' rel_pitch' key_sig' meter_spec' ly_output = 
    prolog +++ body 
  where
    prolog = version lilypond_version +++ header [title score_title']
    body   = book [score (melody rel_pitch'
                                 key_sig'
                                 (fst meter_spec') 
                                 (lilypondOutput ly_output))]


    

data SingleMelodyScoreSkeleton = SingleMelodyScoreSkeleton {
        score_title     :: String,
        key_sig         :: Key,
        meter_spec      :: MetricalSpec,
        rel_pitch       :: Pitch
      }

defaultSingleMelodyScoreSkeleton :: String 
                                 -> Key 
                                 -> MetricalSpec 
                                 -> SingleMelodyScoreSkeleton 
defaultSingleMelodyScoreSkeleton t k m = SingleMelodyScoreSkeleton {
        score_title     = t,
        key_sig         = k,
        meter_spec      = m,
        rel_pitch       = middle_c
      }                          


singleMelodyScoreSkel :: SingleMelodyScoreSkeleton -> M.Part -> Doc
singleMelodyScoreSkel skel mus = unP $ prolog +++ body
  where 
    ly_score  = convertToLyRelative (rel_pitch skel) mus
    ly_output = generateLilyPond (key_sig skel) 
                                 (fst $ meter_spec skel) 
                                 ly_score
    prolog    = version lilypond_version +++ header [title $ score_title skel]
    body      = book [score (melody (rel_pitch skel)
                                    (key_sig skel)
                                    (fst $ meter_spec skel) 
                                    (lilypondOutput ly_output))]


