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
  printLy, writeLy, lilypond_template
  ) where

import Bala.Format.Output.OutputLilyPond 

import Text.PrettyPrint.Leijen

simpledoc :: Ly a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty (unLy e))


printLy :: Ly a -> IO ()
printLy e = putStr ((displayS (simpledoc e) []) ++ "\n") 
    
    
writeLy :: FilePath -> Ly a -> IO ()
writeLy filename e = let sdoc = renderPretty 0.8 80 (pretty (unLy e)) in do
    writeFile filename ((displayS (simpledoc e) []) ++ "\n")


lilypond_template :: String -> Ly b -> Ly CT_Toplevel
lilypond_template s expr = 
    toplevel 
      +++ version "2.10.3" 
      +++ header (headerBlk +++ title s)
      +++ book
            (block (score 
                      (block (relative (_c ! raised 1) expr))))
                      
                      