{-# LANGUAGE MultiParamTypeClasses #-}

-- make sure abcm2ps is in your path.

module DemoOuputAbc where

import Bala.Format.Output.OutputAbc
import Bala.Base.Meter ( (//) )

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen



demo_001 =  tune +++ firstRepeat +++ beginSlur
demo_pp1 = runAbc demo_001

-- double attr application is unfortunately allowed
demo_002 = tune +++ rest ! dur (2 // 1) ! dur (4//1) +++ firstRepeat
demo_pp2 = runAbc demo_002


demo_002a = tune +++ rest +++ firstRepeat

demo_003 = tune +++ sharp !> note C ! octaveHigh 2 +++ firstRepeat 
demo_003a = sharp !> note C

demo_pp3 = runAbc demo_003

-- these two should fail if uncommented
-- demo_003b () = rest ! octaveHigh 2
-- demo_003c () = rest ! flat

demo_004 =        header
              +++ book_field    "My song book" 
              +++ area_field    "area" 
              +++ tempo_field   << stempo (1 // 2) 2 
              +++ meter_field   << meter  (3 // 4)
              +++ key_field     << key_spec c_ locrian
              +++ history_field ["All tunes", "written in", "the past"]
              +++ l_field       (2 // 4)
              +++ words_field   "la di da"
              
              +++ body      << x1             
              +++ body      << words_field "lolalalo"
              +++ body      << x1

  where 
    x1 = tune +++ sharp !> note C  ! octaveHigh 2 +++ firstRepeat 

demo_pp4 = runAbc demo_004


        
demo_005 = key_spec (sharp !> c_ ) locrian
demo_pp5 = runAbc demo_005


bala_test  =        header
                +++ number_field  1
                +++ title_field   "Bala Abc test"
                +++ meter_field   << meter (4 // 4)
                +++ key_field     << key_spec (note C) major
                +++ l_field       (1 // 1)
                +++ body          << e1
  where
    e1 =     tune 
         +++ nplet 2 +++ note C 
         +++ note E  +++ sharp !> note G  +++ z1
         +++ gracenotes [c_, f_, a_] +++ note C +++ z1 
         +++ upbow !> c_ 


               


outputDoc :: Abc a -> FilePath -> FilePath -> IO ()
outputDoc e abcpath pspath = let sdoc = renderPretty 0.8 80 (pretty $ unAbc e) in do
    writeFile abcpath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("abcm2ps " ++ abcpath ++ " -O " ++ pspath)  
    waitForProcess ph
    return ()
  

main = outputDoc bala_test "bala_test.abc" "bala_test.ps"

