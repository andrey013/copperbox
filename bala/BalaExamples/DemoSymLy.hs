

-- Make sure lilypond is in your path

module DemoSymLy where

import Bala.Format.SymLilyPond.LilyPond

import System.Process (runCommand, waitForProcess)
import Text.ParserCombinators.Parsec (parse, parseTest, Parser)
import Text.PrettyPrint.Leijen hiding (dot)


_ces :: (CAccidental repr, CPitch repr, CAttr repr) => repr Pitch
_ces = _c %% flat 


demo_01 () = note _c %% dur 4 
demo_pp1 = printP demo_01

demo_02 ()  = chord [_c,_e,_g] %% dur 4 
demo_pp2 = printP demo_02

demo_03 () = time (3%4)
demo_pp3 = printP demo_03 

demo_03b () = longfermata

demo_04 () = version "2.10.3"
demo_pp4 = printP demo_04

demo_05 () = header (headerCtx +++ title "Bulgarian 6" +++ dedication "unknown")
demo_pp5 = printP demo_05





demo_06 () = elementCtx 
    +++ note (_ces %% raised 1) %% (dur 4 %% dot) %% fingering 4 
    +++ note _ces %% breve

demo_06a () = elementCtx 
    `snoc` note (_ces %% raised 1) %% (dur 4 %% dot) %% fingering 4 
    `snoc` note _ces %% breve

demo_pp6 = printP demo_06


demo_07 () = note _c %% fermata 
demo_pp7 = printP demo_07

-- Snoc list rather than concatenation
demo_08 () = elementCtx `snoc` (note _g) `snoc` (note _c)

-- Correctly fails with a type error
-- pitches_002 () = toplevelCtx `snoc` (note _g) `snoc` (note _c)

demo_pp8 = printP demo_08

demo_09 () = note _c %% dashHat
demo_pp9 = printP demo_09

demo_09b () = note _c  %% (vdefault $ marcato)
demo_pp9b = printP demo_09b

demo_09c () = note _c  %% (vabove $ marcato)
demo_pp9c = printP demo_09c

demo_10 () = (block a) \\ (block b)
  where 
    a = elementCtx +++ note _c +++ note _d +++ note _e +++ note _c 
    b = elementCtx +++ note _g +++ note _b +++ note _g +++ note _b 

demo_pp10 = printP demo_10


merge k xs = foldl fn (block k) xs
  where
    fn acc a = (\\) acc (block a)
  
           
lilypond_test () = 
  toplevelCtx +++ version "2.10.3" 
              +++ header (headerCtx +++ title "Bala LilyPond test")
              +++ block e
  where 
    e = elementCtx +++ relative (_c %% raised 2) 
          (elementCtx +++ key _g major +++ clef treble +++ time (2%4) +++ tempo (duration 4) 120  
           +++ note _g %% dur 8 +++ openBeam +++ note _a %% dur 8 
           +++ note _b %% dur 8 +++ closeBeam +++ note _a %% dur 8  )                


outputDoc :: (() -> P a) -> FilePath -> IO ()
outputDoc e lypath = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile lypath ((displayS sdoc []) ++ "\n")
    ph <- runCommand ("lilypond " ++ lypath)  
    waitForProcess ph
    return ()
    
main = outputDoc lilypond_test "lilypond_test.ly"

--------------------------------------------------------------------------------
-- Test the parser...

para = Para { parsePitch = pitchA
            , parseDuration = pDuration ## pDotted }

lyparse p s = case (parse p "" s) of
                Left err -> print err
                Right val -> putDoc $ unP val
                
mf = parseTest meterFraction "2/3 a"
                

p_demo01    = lyparse (parsePitch para) "aes''"
p_demo01b   = lyparse pitchA "d''"

-- should get *** Exception: Prelude.undefined rather than a parse failure
p_demo02    = lyparse (pRelative para) "\\relative c'' { } "

p_demo03    = lyparse (pChord para) "<c e g>"

p_demo04    = lyparse ((pNote para) ## (pDuration ## pDotted)) "c''4."

-- "\\times 2/3 {c'4 c' c'} " - should get *** Exception: Prelude.undefined
p_demo05    = lyparse (pTimes para) "\\times 2/3 { c' } "



{-
-- should be notes at the elementCtx not pitches

-- pitchList = many1Cat elementCtx pitchA
  
-- p_demo06 = lyparse pitchList "c d e"

-}


p_demo07 = lyparse pCmdArticulation "\\prallprall "





