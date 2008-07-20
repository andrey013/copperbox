--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.AbcInternals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for generating Abc.
--
--------------------------------------------------------------------------------

module Bala.Perform.AbcInternals (
  printAbc, writeAbc, execAbcm2psOn, abc_template, empty_body
  ) where

import Bala.Format.Output.OutputAbc
import Bala.Base.Meter

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen

simpledoc :: Abc a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty (unAbc e))

printAbc :: Abc a -> IO ()
printAbc e = putStr ((displayS (simpledoc e) []) ++ "\n") 
    
    
writeAbc :: FilePath -> Abc a -> IO ()
writeAbc filename e = writeFile filename ((displayS (simpledoc e) []) ++ "\n") 


execAbcm2psOn :: FilePath -> FilePath -> IO ()
execAbcm2psOn abc_filename ps_filename = do
    ph <- runCommand ("abcm2ps " ++ abc_filename ++ " -O " ++ ps_filename)  
    waitForProcess ph
    return ()
    
        
abc_template title expr = 
          header
      +++ number_field  1
      +++ title_field   title      
      +++ meter_field   << meter (4 // 4)
      +++ key_field     << key_spec (note C) major
      +++ key_field     << clef treble
      +++ body          << expr

empty_body = tune
      
      
          