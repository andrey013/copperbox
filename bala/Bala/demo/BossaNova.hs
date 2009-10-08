{-# OPTIONS -Wall #-}


module BossaNova where

import Bala.BeatPattern
import Bala.Mullein

import Mullein.LilyPond hiding ( Duration, rest, makeChord )
import qualified Mullein.NamedElements          as M

import Text.PrettyPrint.Leijen hiding ( dot )

import Data.Ratio


--------------------------------------------------------------------------------


ride_cymbal  :: Fractional a => [BeatS a b]
side_stick   :: Fractional a => [BeatS a b]
bass_drum    :: Fractional a => [BeatS a b]

ride_cymbal  = [ xxxx, xxxx, xxxx, xxxx ]
side_stick   = [ x__x, __x_, __x_, _x__ ]
bass_drum    = [ x__x, x__x, x__x, x__x ]

two_half_notes :: [Rational]
two_half_notes = [2%4, 2%4]


bossa_score :: [DrumGlyph]
bossa_score = 
  makeDrumScore (1%8) (2,two_half_notes) [ (M.ridecymbal, ride_cymbal)
                             , (M.sidestick,  side_stick)
                             , (M.bassdrum,   bass_drum)
                 ] 


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> header [title "Bossa nova"]
     <$> variableDef "bossaNova" 
                     (drummode (time' M.four_four_time <$> stemUp <$> tune))
     <$> book (scoreExpr $ (new "DrumStaff"  ( variableUse "bossaNova")) 
                        <$> layout
                        <$> midi_part)
  where
    tune      = simpleOutput $ renderPhrase lyDrumGlyph
                             $ rewriteDuration xs

    xs        = phrase four4Tm bossa_score

    four4Tm   = [2%4,2%4]
    
    midi_part = midiContextScoreTempo 120 4

output1 :: IO ()
output1 = runLilyPond "bossanova.ly" demo1

