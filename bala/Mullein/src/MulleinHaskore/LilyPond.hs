{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.LilyPond
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output LilyPond
--
--------------------------------------------------------------------------------


module MulleinHaskore.LilyPond ( 
  lilypondVersion,
  MelodyScoreSkeleton(..),
  defaultMelodyScoreSkeleton,
  singleMelodyScoreSkel,
  pianoScoreSkel,
  ) where


import Mullein.Core
import Mullein.LilyPondConvert
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.NamedElements ( middle_c )
import Mullein.Pitch

import qualified Mullein.Score as M

import Text.PrettyPrint.Leijen

lilypondVersion :: String
lilypondVersion = "2.12.2"


    

data MelodyScoreSkeleton = MelodyScoreSkeleton {
        scoreTitle      :: String,
        keySig          :: Key,
        meterSpec       :: MetricalSpec,
        relativePitch   :: Pitch
      }

defaultMelodyScoreSkeleton :: String 
                           -> Key 
                           -> MetricalSpec 
                           -> MelodyScoreSkeleton 
defaultMelodyScoreSkeleton t k m = MelodyScoreSkeleton {
        scoreTitle      = t,
        keySig          = k,
        meterSpec       = m,
        relativePitch   = middle_c
      }                          


singleMelodyScoreSkel :: MelodyScoreSkeleton -> M.Part -> Doc
singleMelodyScoreSkel skel mus = unP $ prolog +++ body
  where 
    ly_score  = convertToLyRelative (relativePitch skel) mus
    ly_output = generateLilyPond (keySig skel) 
                                 (fst $ meterSpec skel) 
                                 ly_score
    prolog    = version lilypondVersion +++ header [title $ scoreTitle skel]
    body      = book [score (melody (relativePitch skel)
                                    (keySig skel)
                                    (fst $ meterSpec skel) 
                                    "treble"
                                    (lilypondOutput ly_output))]


pianoScoreSkel :: MelodyScoreSkeleton -> M.Part -> M.Part -> Doc
pianoScoreSkel skel treble_part bass_part = 
    unP $ prolog +++ treble_decl +++ bass_decl +++ body
  where
    mkPart skore  = generateLilyPond (keySig skel) (fst $ meterSpec skel)    
                        $ convertToLyRelative (relativePitch skel) skore

    treble_ly     = mkPart treble_part 
    bass_ly       = mkPart bass_part

    treble_decl   = definition "treblePart" (melodySkel skel "treble" treble_ly) 
    bass_decl     = definition "bassPart"   (melodySkel skel "bass" bass_ly) 
    
    prolog        = version lilypondVersion +++ header [title $ scoreTitle skel]
    body          = book [score (doubleAngles [ newStaff (usedef "treblePart")
                                              , newStaff (usedef "bassPart")   ])]

melodySkel :: MelodyScoreSkeleton -> String -> LilyPondOutput -> P CtxScore
melodySkel skel clefName lyOut = melody (relativePitch skel)
                                        (keySig skel)
                                        (fst $ meterSpec skel) 
                                        clefName
                                        (lilypondOutput lyOut)