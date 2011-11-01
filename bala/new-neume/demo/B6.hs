{-# OPTIONS -Wall #-}

module B6 where

import Neume.Core.Duration
import Neume.Core.NamedElements
import Neume.Core.Pitch
import Neume.Core.LilyPondMonad


notelist = relative middle_c $ 
        key  a_nat "major"
    >$> time (2,4)     
    >$> clef "treble"
    >$> repeat_volta 2 bar1

demo01 = execLyScore $ 
   version "2.12.2" >$> score notelist
   

bar1 = beam [sn (a_ 4), sn (b_ 4), sn (cs_ 5), sn (cs_ 5)]