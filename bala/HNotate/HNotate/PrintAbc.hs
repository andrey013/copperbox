--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PrintAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output Abc via the Print monad.
--
--------------------------------------------------------------------------------

module HNotate.PrintAbc where

import qualified HNotate.Duration as D
import qualified HNotate.Pitch as P
import HNotate.PrintMonad

import Data.Char
import Data.Ratio
import Text.PrettyPrint.Leijen

-- ** Pitch (4.1)

-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves


note :: P.Pitch -> D.Duration -> PrintM () 
note p d = glyph $ noteD p <> durationD d


noteD :: P.Pitch -> Doc
noteD (P.Pitch l a o) 
    | o > 4     = accidental a $ octave o $ (char . toLower . letter) l
    | otherwise = accidental a $ octave o $ (char . letter) l
  where     
    letter :: P.PitchLetter -> Char
    letter = fn . show
      where fn [x] = x
            fn xs   = error $ "letter " ++ xs 

    accidental :: P.Accidental -> Doc -> Doc 
    accidental P.Nat           = id    
    accidental P.Sharp         = (char '^'  <>)
    accidental P.Flat          = (char '_'  <>)
    accidental P.DoubleSharp   = (text "^^" <>)
    accidental P.DoubleFlat    = (text "__" <>)
   
    octave :: Int -> Doc -> Doc
    octave i  | i > 5       = (<> text (replicate (i-5) '\'')) 
              | i < 4       = (<> text (replicate (4-i) ','))
              | otherwise   = id

duration :: D.Duration -> PrintM ()
duration = attribute . durationD


durationD :: D.Duration -> Doc
durationD d | d == D.no_duration = empty
            | otherwise          = (fn . nd . D.rationalize) d
  where
    nd r     = (numerator r, denominator r)
    fn (n,1) = int n
    fn (1,d) = char '/' <> int d
    fn (n,d) = int n <> char '/' <> int d

rest :: D.Duration -> PrintM ()
rest = glyph . (char 'z' <>) . durationD
    
spacer :: D.Duration -> PrintM ()
spacer = glyph . (char 'x' <>) . durationD

chord :: [P.Pitch] -> D.Duration -> PrintM ()
chord ps = glyph . chordD ps

chordD :: [P.Pitch] -> D.Duration -> Doc
chordD ps d = chord1 ps <> durationD d
  where chord1 = brackets . hcat . map noteD     

gracenotes :: [P.Pitch] -> PrintM ()
gracenotes = glyph . gracenotesD

gracenotesD :: [P.Pitch] -> Doc
gracenotesD = braces . hcat . map noteD  

tie :: PrintM ()
tie = glyph $ char '-'

comment :: String -> PrintM ()
comment s = breakingElement $ text $ '%':' ':s  

barline :: PrintM ()
barline = element $ char '|'

-- voc - voice overlay continuation
voc :: PrintM ()
voc = breakingElement $ text "&\\"


--------------------------------------------------------------------------------
--     
tilde               :: PrintM ()
tilde               = prefix $ char '~'

stacatto            :: PrintM ()
stacatto            = prefix $ char '.'

downbow             :: PrintM ()
downbow             = prefix $ char 'v'

upbow               :: PrintM ()
upbow               = prefix $ char 'u'    