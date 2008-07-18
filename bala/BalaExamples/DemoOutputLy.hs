

-- Make sure lilypond is in your path

module DemoOutputLy where

import Bala.Format.Output.OutputLilyPond
import Bala.Base.Meter ( (//) )

import System.Process (runCommand, waitForProcess)
import Text.ParserCombinators.Parsec (parse, parseTest, Parser)
import Text.PrettyPrint.Leijen hiding (dot)


_ces :: Ly Pitch
_ces = _c ! flat 


demo_01 = note _c ! dur 4 
demo_pp1 = runLy demo_01

demo_02 = chord [_c,_e,_g] ! dur 4 
demo_pp2 = runLy demo_02

demo_03 = time (3//4)
demo_pp3 = runLy demo_03 


demo_03b = longfermata

demo_04 = version "2.10.3"
demo_pp4 = runLy demo_04


demo_05 = header (headerBlk +++ title "Bulgarian 6" +++ dedication "unknown")
demo_pp5 = runLy demo_05





demo_06 = elementBlk 
    +++ note (_ces ! raised 1) ! (dur 4 ! dot) ! fingering 4 
    +++ note _ces ! breve

demo_pp6 = runLy demo_06


demo_07 = note _c ! fermata 
demo_pp7 = runLy demo_07



demo_08 = elementBlk +++ (note _g) +++ (note _c)

-- Correctly fails with a type error
-- pitches_002 = toplevelCtx `snoc` (note _g) `snoc` (note _c)

demo_pp8 = runLy demo_08

demo_09 = note _c ! dashHat
demo_pp9 = runLy demo_09

demo_09b = note _c  ! (defaultPosition marcato)
demo_pp9b = runLy demo_09b

demo_09c = note _c  ! (above marcato)
demo_pp9c = runLy demo_09c


demo_10 = (block a) \\ (block b)
  where 
    a = elementBlk +++ note _c +++ note _d +++ note _e +++ note _c 
    b = elementBlk +++ note _g +++ note _b +++ note _g +++ note _b 

demo_pp10 = runLy demo_10


merge k xs = foldl fn (block k) xs
  where
    fn acc a = (\\) acc (block a)
  
           
lilypond_test = 
  toplevel +++ version "2.10.3" 
           +++ header (headerBlk +++ title "Bala LilyPond test")
           +++ block e
  where 
    e = elementBlk +++ relative (_c ! raised 2) 
          (elementBlk +++ key _g major +++ clef treble 
            +++ time (2//4)      +++ tempo (duration 4) 120  
            +++ note _g ! dur 8 +++ openBeam  +++ note _a ! dur 8 
            +++ note _b ! dur 8 +++ closeBeam +++ note _a ! dur 8  )                


outputDoc :: (Ly a) -> FilePath -> IO ()
outputDoc e lypath = let sdoc = renderPretty 0.8 80 (pretty $ unLy e) in do
    writeFile lypath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("lilypond " ++ lypath)  
    waitForProcess ph
    return ()
    
main = outputDoc lilypond_test "lilypond_test.ly"


