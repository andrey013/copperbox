{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ghci ...
-- :set -i../src:../../Mullein/src


module BossaNova where

import Bala.BalaMullein
import Bala.BeatPattern

import Mullein.LilyPond hiding ( Duration, rest, makeChord, B )
import qualified Mullein.NamedElements          as M

import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio


instance InterpretRest DrumGlyph where
  interpretRest = mkRest

--------------------------------------------------------------------------------

cymbal_pattern :: [Beat Rational]
cymbal_pattern = run1 (4%4) $ times 2 $ beats [1,1,1,1] >< beats [1,1,1,1]


sidestick_pattern :: [Beat Rational]
sidestick_pattern = run1 (4%4) patt where
  patt = beat 1 >< rest 2 >< beat 1 >< rest 2 >< beat 1 >< rest 1 //
         rest 2 >< beat 1 >< rest 2 >< beat 1 >< rest 2


bassdrum_pattern :: [Beat Rational]
bassdrum_pattern = run1 (4%4) $ times 2 patt where
  patt = beat 1 >< rest 2 >< beats [1,1] >< rest 2 >< beat 1


temp_cyms :: [DrumGlyph]
temp_cyms = zipInterp (replicate 16 mkCym) cymbal_pattern where
  mkCym = mkDrumNote M.ridecymbal

-- TODO - amalgamate the three drum patterns into drum chords (how?)


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> header [title "bossa nova"]
     <$> variableDef "bossaNova" (drummode (time 4 4 <$> stemUp <$> tune))
     <$> book (score (new "DrumStaff"  ( variableUse "bossaNova")))
  where
    tune      = simpleOutput $ renderPhrase 
                             $ rewriteDuration xs

    xs        = phrase four4Tm temp_cyms

    four4Tm   = [2%4,2%4]

output1 :: IO ()
output1 = runLilyPond "bossanova.ly" demo1

