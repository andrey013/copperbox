

module DemoSymLy where

import Bala.Format.SymLilyPond.LilyPond

import System.Process (runCommand, waitForProcess)
import Text.PrettyPrint.Leijen hiding (dot)


_ces :: (SymAttrAccidental repr, SymPitch repr) => repr (Pitch ctx)
_ces = _c # sharp 


demo_01 () = note _c # dur 4 
demo_pp1 = printP demo_01

demo_02 ()  = chord [_c,_e,_g] # dur 4 
demo_pp2 = printP demo_02

demo_03 () = cmdTime (3%4)
demo_pp3 = printP demo_03 

demo_03b () = longfermata

demo_04 () = version "2.10.3"
demo_pp4 = printP demo_04

demo_05 () = header [title "Bulgarian 6", dedication "unknown"]
demo_pp5 = printP demo_05

demo_06 () = note (_ces # raised 1) # dot # fingering 4 +++ note _ces # breve
demo_pp6 = printP demo_06

demo_07 () = note _c # fermata 
demo_pp7 = printP demo_07

lilypond_test () = version "2.10.3" +++ header [title "Bala LilyPond test"]
                   +++ block e
  where 
    e = note _c # dur 4 +++ note _d # dur 4 +++ note _e # dur 4
                      





outputDoc :: (() -> P a) -> FilePath -> IO ()
outputDoc e lypath = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile lypath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("/usr/local/bin/lilypond " ++ lypath)  
    waitForProcess ph
    return ()
    
main = outputDoc lilypond_test "lilypond_test.ly"  