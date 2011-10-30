{-# OPTIONS -Wall #-}

module B6 where

import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.LilyPondMonad


notelist = relative middle_c $ beam $ 
    [note middle_c dEighth, note middle_c dEighth]

demo01 = execLyScore $ 
   version "2.12.2" >> score notelist
   