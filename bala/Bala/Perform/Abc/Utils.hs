
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Abc.Utils
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

module Bala.Perform.Abc.Utils (
  printAbc,
  writeAbc,
  performanceToAbc,
  scoreToAbc
  ) where

import Bala.Format.Output.OutputAbc
import Bala.Perform.Abc.AbcBackend (generateAbcScore, default_abc_env)
import Bala.Perform.Abc.ToAbcScore (abcscore)
import Bala.Perform.Base.Class (Perform)
import Bala.Perform.Base.EventTree (Performance)
import Bala.Perform.Base.Utils (displaySimple)
import Bala.Perform.Score.Datatypes (ScScore)
import Bala.Perform.Score.ToScore (toScore, default_score_env)


import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen

printAbc :: Abc a -> IO ()
printAbc (Abc a) = putStr $ displaySimple $ pretty a

writeAbc :: FilePath -> Abc a -> IO ()
writeAbc filename (Abc a) = writeFile filename (displaySimple $ pretty a)


performanceToAbc :: (Perform evt) 
                  => Performance evt 
                  -> [AbcCxt_Body]
performanceToAbc perf = let sc0   = toScore perf default_score_env
                            abc   = abcscore sc0
                        in generateAbcScore abc default_abc_env
              
scoreToAbc :: ScScore -> [AbcCxt_Body]  
scoreToAbc sc = generateAbcScore (abcscore sc) default_abc_env
