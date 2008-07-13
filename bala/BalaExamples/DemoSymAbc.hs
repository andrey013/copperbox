{-# LANGUAGE MultiParamTypeClasses #-}

-- make sure abcm2ps is in your path.

module DemoSymAbc where

import Bala.Format.SymAbc.AbcFormat
import Bala.Base.Meter ( (//) )

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen


demo_001 () =  elementCtx +++ firstRepeat +++ beginSlur
demo_pp1 = printP demo_001

-- double attr application is unfortunately allowed
demo_002 () = elementCtx +++ rest %% dur (2 // 1) %% dur (4//1) +++ firstRepeat
demo_pp2 = printP demo_002


demo_002a () = elementCtx +++ rest +++ firstRepeat

demo_003 () = elementCtx +++ sharp -%- note C %% octaveHigh 2 +++ firstRepeat 
demo_003a () =  sharp -%- note C

demo_pp3 = printP demo_003

-- these two should fail if uncommented
-- demo_003b () = rest %% octaveHigh 2
-- demo_003c () = rest %% flat

demo_004 () =     fieldCtx
              +++ book_field    "My song book" 
              +++ area_field    "area" 
              +++ tempo_field   << stempo (1 // 2) 2 
              +++ meter_field   << meter  (2 // 3)
              +++ key_field     << key << keySpec c_ %% locrian
              +++ history_field ["All tunes", "written in", "the past"]
              +++ l_field       (2 // 4)
              +++ words_field   "la di da"
              
              +++ tunebody      << elements << x1             
              +++ tunebody      << midtuneField << words_field "lolalalo"
              +++ tunebody      << elements << x1

  where 
    x1 = elementCtx +++ sharp -%- note C  %% octaveHigh 2 +++ firstRepeat 

demo_pp4 = printP demo_004

        
demo_005 () = keySpec (sharp -%- c_ ) %% locrian
demo_pp5 = printP demo_005


bala_test () =      fieldCtx
                +++ number_field  1
                +++ title_field   "Bala Abc test"
                +++ meter_field   << meter (4 // 4)
                +++ key_field     << key << keySpec << note C
                +++ l_field       (1 // 1)
                +++ tunebody      << elements << e1
  where
    e1 = elementCtx +++ nplet 2 +++ note C +++ note E 
                                +++ sharp -%- note G  +++ z1
          +++ gracenotes [c_, f_, a_] +++ note C +++ z1 
          +++ c_ %% upbow


               


outputDoc :: (() -> P a) -> FilePath -> FilePath -> IO ()
outputDoc e abcpath pspath = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile abcpath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("abcm2ps " ++ abcpath ++ " -O " ++ pspath)  
    waitForProcess ph
    return ()
  

main = outputDoc bala_test "bala_test.abc" "bala_test.ps"

