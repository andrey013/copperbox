{-# LANGUAGE MultiParamTypeClasses #-}

module DemoSymAbc where

import Bala.Format.SymAbc.AbcFormat

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen



demo_001 () =  firstRepeat +++ beginSlur


-- double attr application is unfortunately allowed
demo_002 () = rest # dur 2 # dur 4 +++ firstRepeat

demo_002a () = rest +++ firstRepeat

demo_003 () = note C # sharp # octaveHigh 2 +++ firstRepeat 
demo_003a () = sharp (note C) 

-- these two should fail if uncommented
-- demo_003b () = rest # octaveHigh 2
-- demo_003c () = rest # flat

demo_004 () =     book_ "My song book" 
              +++ area_ "area" 
              +++ tempo_ << stempo (1%2) 2 
              +++ meter_ << meter  (2%3)
              +++ key_ << key << keySpec c_ # locrian # dorian
              +++ history_ ["All tunes", "written in", "the past"]
              +++ defaultLength_ (2%4)
              +++ words_ "la di da"
              
              +++ abcmusic << elements << x1             
              +++ abcmusic << midtuneField << words_ "lolalalo"
              +++ abcmusic << elements << x1

  where 
    x1 = note C # sharp # octaveHigh 2 +++ firstRepeat 
              
demo_005 () = keySpec (c_ # sharp) # locrian


bala_test () =      num_ 1
                +++ title_ "Bala Abc test"
                +++ meter_ << meter (4%4)
                +++ key_ << key << keySpec << note C
                +++ defaultLength_ (1%1)
                +++ abcmusic << elements << e1
  where
    e1 = nplet 2 +++ note C +++ note E +++ note G # sharp +++ z1
          +++ gracenotes [c_, f_, a_] << note C +++ z1 
          +++ upbow << c_


               

outputDoc :: (() -> P a) -> FilePath -> FilePath -> IO ()
outputDoc e abcpath pspath = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile abcpath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("/usr/local/bin/abc2ps " ++ abcpath ++ " -O " ++ pspath)  
    waitForProcess ph
    return ()
  

main = outputDoc bala_test "bala_test.abc" "bala_test.ps"

