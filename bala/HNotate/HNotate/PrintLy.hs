--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PrintLy
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output LilyPond via the Print monad.
--
--------------------------------------------------------------------------------

module HNotate.PrintLy where

import qualified HNotate.Duration as D
import qualified HNotate.Pitch as P
import HNotate.PrintMonad

import Data.Char
import Text.PrettyPrint.Leijen

commandD :: String -> Doc
commandD = text . ('\\':)

command0 = element . commandD

command1 s d = element $ commandD s <+> d


note :: P.Pitch -> D.Duration -> PrintM ()
note p d = glyph $ pitchD p <> durationD d

pitchD :: P.Pitch -> Doc
pitchD (P.Pitch l a o) = octave o $ accidental a $ (char . toLower . letter) l
  where
    letter :: P.PitchLetter -> Char
    letter = fn . show
      where fn [x] = x
            fn xs  = error $ "letter " ++ xs 

    accidental :: P.Accidental -> Doc -> Doc
    accidental P.Nat            = id
    accidental P.Sharp          = (<> text "is")
    accidental P.Flat           = (<> text "es")
    accidental P.DoubleSharp    = (<> text "isis")
    accidental P.DoubleFlat     = (<> text "eses")
     
    octave :: Int -> Doc -> Doc
    octave i | i > 0            = (<> text (replicate i '\''))
             | i < 0            = (<> text (replicate i ','))
             | otherwise        = id 
    
    
durationD :: D.Duration -> Doc
durationD drn
    | drn == D.no_duration = empty
    | otherwise            = let (n,d,dc) = D.pdElements $ D.printableDuration drn 
                             in dots dc $ durn n d
  where 
    durn 4 1      = commandD "longa"  
    durn 2 1      = commandD "breve" 
    durn 1 i      = int i
    durn n d      = error $ "durationD failed on - " ++ 
                                  show n ++ "%" ++ show d
    
    dots :: Int -> Doc -> Doc
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id
        

rest :: D.Duration -> PrintM ()
rest = glyph . (char 'r' <>) . durationD
    
spacer :: D.Duration -> PrintM ()
spacer = glyph . (char 's' <>) . durationD

chord :: [P.Pitch] -> D.Duration -> PrintM ()
chord ps = glyph . chordD ps

chordD :: [P.Pitch] -> D.Duration -> Doc
chordD ps d = chord1 ps <> durationD d
  where chord1 = angles . hsep . map pitchD     

gracenotes :: [P.Pitch] -> PrintM ()
gracenotes ps = command0 "grace" >> (glyph $ gracenotesD ps)

gracenotesD :: [P.Pitch] -> Doc
gracenotesD = braces . hsep . map pitchD  

polystart :: PrintM ()
polystart = element $ text "<<"

polyend :: PrintM ()
polyend = element $ text ">>"

polyc :: PrintM ()
polyc = element $ text "\\\\"
        
barNumberCheck :: Int -> PrintM ()
barNumberCheck i = command1 "barNumberCheck" (text $ '#':show i) >> manualbreak


barcheck :: PrintM ()
barcheck = element $ char '|'

tie :: PrintM ()
tie = element $ char '~'

openbrace :: PrintM ()
openbrace = element $ char '{'

closebrace :: PrintM ()
closebrace = element $ char '}'

openbeam :: PrintM ()
openbeam = element $ char '['

closebeam :: PrintM ()
closebeam = element $ char ']'