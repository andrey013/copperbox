{-# OPTIONS -Wall #-}

module B6 where

import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.LilyPondMonad


demo01 = runLilyPondM $ relNote middle_c dHalf