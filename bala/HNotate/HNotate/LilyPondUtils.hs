
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

module HNotate.LilyPondUtils (
    printLy,
    writeLy,
    systemToLy,
    scoreToLy,
    runLilyPondOn
  ) where


import HNotate.Backend.LilyPond.LyBackend (generateLilyPondScore, default_ly_env)
import HNotate.Backend.LilyPond.LyFragments
import HNotate.Backend.LilyPond.ToLyScore (lyscore)
import HNotate.Base.Class (Event)
import HNotate.Base.EventTree (System)
import HNotate.Base.Utils (displaySimple)
import HNotate.Print.OutputLilyPond
import HNotate.Score.Datatypes (ScScore)
import HNotate.Score.ToScore (toScore, default_score_env)
import HNotate.System.SystemLilyPond

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen


printLy :: Ly a -> IO ()
printLy (Ly a) = putStr $ displaySimple $ pretty a

writeLy :: FilePath -> Ly a -> IO ()
writeLy filename (Ly a) = writeFile filename (displaySimple $ pretty a)


systemToLy :: (Event evt) => LilyPondSystem -> System evt -> LilyPondScore
systemToLy sys evts = 
    let sc0  = toScore evts default_score_env
        ly   = lyscore sc0
    in sys $ generateLilyPondScore ly default_ly_env

scoreToLy :: LilyPondSystem -> ScScore -> LilyPondScore
scoreToLy sys sc = sys $ generateLilyPondScore (lyscore sc) default_ly_env

runLilyPondOn :: FilePath -> IO ()
runLilyPondOn filename = do
    ph <- runCommand ("lilypond " ++ filename)  
    waitForProcess ph
    return ()
    
