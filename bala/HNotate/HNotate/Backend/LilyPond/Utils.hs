
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.Utils
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

module HNotate.Backend.LilyPond.Utils (
    printLy,
    writeLy,
    systemToLy,
    scoreToLy
  ) where

import HNotate.Backend.LilyPond.LyBackend (generateLilyPondScore, default_ly_env)
import HNotate.Backend.LilyPond.ToLyScore (lyscore)
import HNotate.Base.Class (Event)
import HNotate.Base.EventTree (System)
import HNotate.Base.Utils (displaySimple)
import HNotate.Print.OutputLilyPond
import HNotate.Score.Datatypes (ScScore)
import HNotate.Score.ToScore (toScore, default_score_env)

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen


printLy :: Ly a -> IO ()
printLy (Ly a) = putStr $ displaySimple $ pretty a

writeLy :: FilePath -> Ly a -> IO ()
writeLy filename (Ly a) = writeFile filename (displaySimple $ pretty a)


systemToLy :: (Event evt) => System evt -> [LyCmdScore]
systemToLy sys = 
    let sc0  = toScore sys default_score_env
        ly   = lyscore sc0
    in generateLilyPondScore ly default_ly_env

scoreToLy :: ScScore -> [LyCmdScore]
scoreToLy sc = generateLilyPondScore (lyscore sc) default_ly_env


