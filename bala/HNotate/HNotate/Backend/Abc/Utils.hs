
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Abc.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for Abc.
--
--------------------------------------------------------------------------------

module HNotate.Backend.Abc.Utils (
    printAbc,
    writeAbc,
    systemToAbc,
    scoreToAbc
  ) where


import HNotate.Backend.Abc.AbcBackend (generateAbcScore, default_abc_env)
import HNotate.Backend.Abc.ToAbcScore (abcscore)
import HNotate.Base.Class (Event)
import HNotate.Base.EventTree (System)
import HNotate.Base.Utils (displaySimple)
import HNotate.Print.OutputAbc
import HNotate.Score.Datatypes (ScScore)
import HNotate.Score.ToScore (toScore, default_score_env)

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen

printAbc :: Abc a -> IO ()
printAbc (Abc a) = putStr $ displaySimple $ pretty a

writeAbc :: FilePath -> Abc a -> IO ()
writeAbc filename (Abc a) = writeFile filename (displaySimple $ pretty a)


systemToAbc :: (Event evt) => System evt -> [AbcCxt_Body]
systemToAbc sys = 
    let sc0   = toScore sys default_score_env
        abc   = abcscore sc0
    in generateAbcScore abc default_abc_env

scoreToAbc :: ScScore -> [AbcCxt_Body]
scoreToAbc sc = generateAbcScore (abcscore sc) default_abc_env
