--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPondInternals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for generating LilyPond.
--
--------------------------------------------------------------------------------

module Bala.Perform.LilyPondInternals (
  printLy, writeLy, execLilyPondOn, lilypond_template
  ) where

import Bala.Format.Output.OutputLilyPond 

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen

simpledoc :: Ly a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty (unLy e))


printLy :: Ly a -> IO ()
printLy e = putStr ((displayS (simpledoc e) []) ++ "\n") 
    
    
writeLy :: FilePath -> Ly a -> IO ()
writeLy filename e = writeFile filename ((displayS (simpledoc e) []) ++ "\n")

-- | execLilyPondOn infile outfile - don't add extension to outfile
-- lilypond will suffix @.ps@ and @.pdf@.
execLilyPondOn :: FilePath -> IO ()
execLilyPondOn filename = do
    ph <- runCommand ("lilypond " ++ filename)  
    waitForProcess ph
    return ()
    
    

lilypond_template :: String -> Ly b -> LyCxt_Toplevel
lilypond_template s expr = 
    toplevelStart
      +++ version "2.10.3" 
      +++ header (headerStart +++ title s)
      +++ book
            (block (score 
                      (block (relative (_c ! raised 1) expr))))
                      
                      