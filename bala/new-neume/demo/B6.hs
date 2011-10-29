{-# OPTIONS -Wall #-}

module B6 where

import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.LilyPondMonad


demo01 = runLilyPondM $ beam $ 
    note middle_c dEighth >> note middle_c dEighth

demo02 = execLyScore $ 
   version "2.12.2"