{-# LANGUAGE MultiParamTypeClasses #-}

-- make sure abc2ps is in your path.

module DemoSymAbc where

import Bala.Format.SymAbc.AbcFormat

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen


demo_001 () =  elementCtx +++ firstRepeat +++ beginSlur
demo_pp1 = printP demo_001

-- double attr application is unfortunately allowed
demo_002 () = elementCtx +++ rest %% dur 2 %% dur 4 +++ firstRepeat
demo_pp2 = printP demo_002


demo_002a () = elementCtx +++ rest +++ firstRepeat

demo_003 () = elementCtx +++ note C %% sharp %% octaveHigh 2 +++ firstRepeat 
demo_003a () = note C `attr` sharp

demo_pp3 = printP demo_003

-- these two should fail if uncommented
-- demo_003b () = rest %% octaveHigh 2
-- demo_003c () = rest %% flat

demo_004 () =     fieldCtx
              +++ book_ "My song book" 
              +++ area_ "area" 
              +++ tempo_ << stempo (1%2) 2 
              +++ meter_ << meter  (2%3)
              +++ key_ << key << keySpec c_ %% locrian %% dorian
              +++ history_ ["All tunes", "written in", "the past"]
              +++ defaultNoteLength_ (2%4)
              +++ words_ "la di da"
              
              +++ abcmusic << elements << x1             
              +++ abcmusic << midtuneField << words_ "lolalalo"
              +++ abcmusic << elements << x1

  where 
    x1 = elementCtx +++ note C %% sharp %% octaveHigh 2 +++ firstRepeat 

demo_pp4 = printP demo_004

        
demo_005 () = keySpec (c_ %% sharp) %% locrian
demo_pp5 = printP demo_005


bala_test () =      fieldCtx
                +++ num_ 1
                +++ title_ "Bala Abc test"
                +++ meter_ << meter (4%4)
                +++ key_ << key << keySpec << note C
                +++ defaultNoteLength_ (1%1)
                +++ abcmusic << elements << e1
  where
    e1 = elementCtx +++ nplet 2 +++ note C +++ note E +++ note G %% sharp +++ z1
          +++ note C %% gracenotes [c_, f_, a_] +++ z1 
          +++ c_ %% upbow


               


outputDoc :: (() -> P a) -> FilePath -> FilePath -> IO ()
outputDoc e abcpath pspath = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile abcpath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("abc2ps " ++ abcpath ++ " -O " ++ pspath)  
    waitForProcess ph
    return ()
  

main = outputDoc bala_test "bala_test.abc" "bala_test.ps"

