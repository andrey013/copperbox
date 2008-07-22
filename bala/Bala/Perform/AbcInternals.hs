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
  printAbc, writeAbc, execAbcm2psOn, 
  Abc_Header_Defaults(..), abc_header_defaults, 
  abc_template, empty_body
  ) where


import qualified Bala.Base as Base
import Bala.Base.Meter 
import Bala.Format.Output.OutputAbc
import Bala.Perform.RenderAbc (abcPitch)

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


data Abc_Header_Defaults = Abc_Header_Defaults {
    abc_title       :: String,
    abc_meter       :: MeterFraction,
    abc_key         :: Base.Key
  }
  deriving (Show)

abc_header_defaults = Abc_Header_Defaults {
    abc_title       = "",
    abc_meter       = 4//4,
    abc_key         = Base.c_major
  }     
        
abc_template defaults expr = 
          header
      +++ number_field  1
      +++ title_field   (abc_title defaults)      
      +++ meter_field   << meter (abc_meter defaults)
      +++ key_field     << key_spec (note C) major
      +++ key_field     << clef treble
      +++ body          << expr
  where
    toKeySpec :: Base.Key -> Abc Key
    toKeySpec k = let (pn,kt) = Base.unKey k
                      pch     = abcPitch $ Base.pitch pn 4 
                  in key_spec pch (toKeyType kt)
    toKeyType (Base.MajorKey) = major
    toKeyType (Base.MinorKey) = minor
    
empty_body = tune
      
      
          