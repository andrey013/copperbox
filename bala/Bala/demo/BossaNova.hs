{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ghci ...
-- :set -i../src:../../Mullein/src


module BossaNova where

import Bala.BalaMullein
import Bala.BeatPattern

import Mullein.LilyPond hiding ( Duration, rest, makeChord )
import qualified Mullein.NamedElements          as M

import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio


instance InterpretRest DrumGlyph where
  interpretRest = mkRest

--------------------------------------------------------------------------------

ridecymbal_pattern :: BeatPattern
ridecymbal_pattern = times 2 $ beats [1,1,1,1] >< beats [1,1,1,1]


sidestick_pattern :: BeatPattern
sidestick_pattern =
  beat 1 >< rest 2 >< beat 1 >< rest 2 >< beat 1 >< rest 1 //
  rest 2 >< beat 1 >< rest 2 >< beat 1 >< rest 2


bassdrum_pattern :: BeatPattern
bassdrum_pattern = times 2 patt where
  patt = beat 1 >< rest 2 >< beats [1,1] >< rest 2 >< beat 1

bossa_score :: [DrumGlyph]
bossa_score = 
  makeDrumScore (4%4) (1%8) [M.ridecymbal, M.sidestick, M.bassdrum] 
                            [ridecymbal_pattern, sidestick_pattern, 
                             bassdrum_pattern]


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> header [title "Bossa nova"]
     <$> variableDef "bossaNova" (drummode (time 4 4 <$> stemUp <$> tune))
     <$> book (score $ (new "DrumStaff"  ( variableUse "bossaNova")) 
                    <$> layout
                    <$> midi_part)
  where
    tune      = simpleOutput $ renderPhrase lyDrumGlyph
                             $ rewriteDuration xs

    xs        = phrase four4Tm bossa_score

    four4Tm   = [2%4,2%4]
    
    midi_part = midiExpr $ contextExpr (command "Score" <$> 
                                         (schemeDef "tempoWholesPerMinute" 
                                                    "ly:make-moment 120 4"))


output1 :: IO ()
output1 = runLilyPond "bossanova.ly" demo1

