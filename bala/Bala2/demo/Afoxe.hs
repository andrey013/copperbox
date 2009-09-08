{-# OPTIONS -Wall #-}

-- ghci ...
-- :set -i../src:../../Mullein/src


module Afoxe where

import Bala.BalaMullein
import Bala.BeatPattern
import Bala.Chord
import Bala.Duration
import Bala.Interval
import Bala.NamedPitches

import Mullein.LilyPond hiding ( Duration, rest, makeChord, B )
import qualified Mullein.NamedElements          as M

import Data.AffineSpace
import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio

-- Reverse application

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


--------------------------------------------------------------------------------
-- chords

c6over9 :: Chord
c6over9 = makeChord c5 [perfect1, major3, major6, major9]

a7sharp5 :: Chord
a7sharp5 = makeChord a4 [perfect1, minor7, major3 # addOve, minor6 # addOve]

dmin9 :: Chord
dmin9 = minor d5 # no5 # min9

g13 :: Chord
g13 = makeChord g4 [perfect1, minor7, major3 # addOve, major6 # addOve]


--------------------------------------------------------------------------------

afoxe_upper :: [Beat Rational]
afoxe_upper = run1 (2%4) $ patt where
  patt = times 4 $ rest 1 >< beats [2,1] >< rest 1 >< beats [2,1]

afoxe_lower :: [Beat Rational] 
afoxe_lower = rewriteRests $ run1 (2%4) afoxe_lower_patt
  where
   rewriteRests = anarewrite 1 fn where
     fn (R a) = B a
     fn a     = a

anarewrite :: Int -> (a -> a) -> [a] -> [a]
anarewrite i f xs = ys ++ map f zs where
  (ys,zs) = splitAt i xs

afoxe_lower_patt :: BeatPattern
afoxe_lower_patt = times 4 $ rest 4 >< beats [2,2]

zipInterp :: MakeRest e => [Duration -> e] -> [Beat Rational] -> [e]
zipInterp fs     (R n:ys) = mkRest n : zipInterp fs ys
zipInterp (f:fs) (B n:ys) = f n : zipInterp fs ys
zipInterp _      _        = []

chordList :: [Chord]
chordList = [c6over9, a7sharp5, dmin9, g13]

afoxeUBuilder :: [Duration -> PDGlyph]
afoxeUBuilder = nrotate 1 $ ntimes 4 $ map upper3 chordList where
  upper3 = mkChord . chordPitches . noRoot

ntimes :: Int -> [a] -> [a]
ntimes i = concat . map (replicate i)

nrotate :: Int -> [a] -> [a]
nrotate i xs = let (h,t) = splitAt i xs in t++ h

afoxeLBuilder :: [Duration -> PDGlyph]
afoxeLBuilder = zipWith ($) funs $ nrotate 2 (ntimes 3 chordList) where
  funs = [mv,  tied,fn,fn, tied,fn,mv, tied,fn,fn, fn]
  fn   = mkNote . chordRoot
  mv   = mkNote . (.-^ (makeInterval 5 5)) . chordRoot
  tied = (setTied .) . mkNote . chordRoot


afoxeU :: [PDGlyph]
afoxeU = zipInterp afoxeUBuilder afoxe_upper

afoxeL :: [PDGlyph]
afoxeL = zipInterp afoxeLBuilder afoxe_lower 

demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative M.middle_c $ key M.c_nat "major" <$> time 2 4 <$> tune)
  where
    tune    = simpleOutput $ renderPhrase 
                           $ rewritePitch M.middle_c 
                           $ rewriteDuration xs
    xs      :: Phrase PDGlyph
    xs      = overlayPhrases (phrase two4Tm afoxeU) (phrase two4Tm afoxeL)

    two4Tm  = makeMeterPattern 2 4

output1 :: IO ()
output1 =  writeDoc "afoxe.ly"  demo1

 


---------------------------------------------------------------------------------

ex32 :: BeatPattern
ex32 = rest 1 >< beats [1,1,1] >< beats [1,1,1,1]         //
       rest 1 >< beats [1,1,1] >< rest 1 >< beats [1,1,1] //
       rest 1 >< beats [1,1,1] >< rest 1 >< beats [2,1]   //
       rest 1 >< beats [1,1,1] >< beats [1,2,1]           //
       beat 4 >< rest 4

demoZ1 :: [Beat Rational]
demoZ1 = run1 (2%4) ex32
