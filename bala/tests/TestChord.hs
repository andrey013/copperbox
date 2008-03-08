
module TestChord where

import Bala
import Text.ParserCombinators.Parsec


c_fp = fixedPitch (read "C4")


cmajor :: IO () 
cmajor = parseTest (many (lexeme romanChord)) $ 
  "I ii iii IV V vi viio I"

triad01 = triad (read "I") ()
triad02 = triad (read "vi") ()

test01 = tip (read "I")

test02 = buildChord (read "C4") (IntervalPattern [5,4])

test03 = buildChord (read "C4") (IntervalPattern [4,5])

test04 = scanl shiftyPlus 10 [5,4]

test04' = scanl fn 10 [5,4]
  where fn a b = a + b - 1

test05 :: GuitarChord
test05 = read "G7"

test06 :: GuitarChord
test06 = read "A/Ab"     

demo = mapM (parseTest readGuitarChord) $ 
  [ -- major
    "Cmaj", "Cma",  "CM" 
  , "C6",  "Cmaj6", "Cma6"
  , "Cmaj7", "Cma7", "CM7", "Cj7"
  , "Cmaj9", "Cma9", "CM9", "Cj9"
  , "Cmaj11", "CM11", "Cj11"
  , "Cmaj13", "CM13", "Cj13"
  , "Cadd9", "C/9"
  , "C6/9", "C9/6" 
  -- minor
  , "Cm", "Cmin", "Cmi", "C-"
  , "Cm6", "Cmin6"
  , "Cm7", "Cmin7"
  , "Cm9", "Cmin9"
  , "Cm11", "Cmin11"
  , "Cm13", "Cmin13"
  , "Cm(maj7)", "CmM7"
  , "Cm(maj9)", "CmM9"
  , "Cm(add9)", "Cm/9"
  , "Cm6/9", "Cm9/6"

-- dominant
  , "C7", "C9", "C11", "C13"
  
-- diminished 
  , "Cdim", "Cdim7", "Cm7b5", "Cm7-5"

-- augmented
  , "Caug", "C+", "C+5"
  , "Caug7", "C7#5", "C7+5"

-- suspended
  , "Csus", "Csus4"
  , "C7sus", "C7sus4"
  , "Csus2"


  ]
     