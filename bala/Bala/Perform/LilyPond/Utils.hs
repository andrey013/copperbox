
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPond.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for LilyPond.
--
--------------------------------------------------------------------------------

module Bala.Perform.LilyPond.Utils (
  printLy,
  writeLy,
  performanceToLy,
  scoreToLy
  ) where

import Bala.Format.Output.OutputLilyPond
import Bala.Perform.Base.Class (Perform)
import Bala.Perform.Base.EventTree (Performance)
import Bala.Perform.Base.Utils (displaySimple)
import Bala.Perform.LilyPond.LyBackend (generateLilyPondScore, default_ly_env)
import Bala.Perform.LilyPond.ToLyScore (lyscore)
import Bala.Perform.Score.Datatypes (ScScore)
import Bala.Perform.Score.ToScore (toScore, default_score_env)

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen


printLy :: Ly a -> IO ()
printLy (Ly a) = putStr $ displaySimple $ pretty a

writeLy :: FilePath -> Ly a -> IO ()
writeLy filename (Ly a) = writeFile filename (displaySimple $ pretty a)


performanceToLy :: (Perform evt) => Performance evt -> [LyCmdScore]
performanceToLy perf = let sc0  = toScore perf default_score_env
                           ly   = lyscore sc0
                       in generateLilyPondScore ly default_ly_env
              
scoreToLy :: ScScore -> [LyCmdScore]
scoreToLy sc = generateLilyPondScore (lyscore sc) default_ly_env


