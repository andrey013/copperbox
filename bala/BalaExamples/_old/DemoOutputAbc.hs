
-- make sure abcm2ps is in your path.

module DemoOutputAbc where

import HNotate.Backend.Abc (printAbc, writeAbc)
import HNotate.Print.OutputAbc


import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen



demo_001 =  body +++ firstRepeat +++ beginSlur
demo_pp1 = runAbc demo_001

-- double attr application is unfortunately allowed
demo_002 = body +++ rest ! dur (2,1) ! dur (4,1) +++ firstRepeat
demo_pp2 = runAbc demo_002


demo_002a = body +++ rest +++ firstRepeat

demo_003 = body +++ sharp !> note C ! octaveHigh 2 +++ firstRepeat 
demo_003a = sharp !> note C

demo_pp3 = runAbc demo_003

-- these two should fail if uncommented
-- demo_003b () = rest ! octaveHigh 2
-- demo_003c () = rest ! flat

demo_004 = tune h1 b1

  where
    h1 = header +++ book_field    "My song book" 
                +++ area_field    "area" 
                +++ tempo_field   << stempo (1,2) 2 
                +++ meter_field   << meter  (3,4)
                +++ key_field     << key_spec c_ locrian
                +++ history_field ["All tunes", "written in", "the past"]
                +++ l_field       (2,4)
                +++ words_field   "la di da"
              
    b1 = body +++ sharp !> note C  ! octaveHigh 2 +++ firstRepeat 

    
demo_pp4 = runAbc demo_004


        
demo_005 = key_spec (sharp !> c_ ) locrian
demo_pp5 = runAbc demo_005


abc_test  = tune   (     header
                      +++ number_field  1
                      +++ title_field   "Bala Abc test"
                      +++ meter_field   << meter (4,4)
                      +++ key_field     << key_spec (note C) major
                      +++ l_field       (1,1)
                    )
                    e1
  where
    e1 =     body 
         +++ nplet 2 +++ note C 
         +++ note E  +++ sharp !> note G  +++ z1
         +++ gracenotes [c_, f_, a_] +++ note C +++ z1 
         +++ upbow !> c_ 


               


outputDoc :: Abc a -> FilePath -> FilePath -> IO ()
outputDoc e abcpath pspath = do
    writeAbc abcpath e
    ph <- runCommand ("abcm2ps " ++ abcpath ++ " -O " ++ pspath)  
    waitForProcess ph
    return ()
  
demo = printAbc abc_test

main = outputDoc abc_test "abc_test.abc" "abc_test.ps"

