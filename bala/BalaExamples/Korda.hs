

--------------------------------------------------------------------------------
-- |
-- Module      :  Korda
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Guitar chords / fingering positions
-- |
--------------------------------------------------------------------------------


module Main where

import Bala

import Data.List
import Data.Maybe
import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 



type FingerPos = Maybe Int
  
data Fingering = Fingering (Maybe Int) [FingerPos] 



 
standard_tuning :: [Pitch]    
standard_tuning = scanl addSemi lower_e [5,5,5,4,5]
  where lower_e :: Pitch
        lower_e = decouper "E3"

withStdTuning = (flip evalFingering) standard_tuning

evalFingering :: Fingering -> [Pitch] -> [Pitch]
evalFingering fin tuning = 
  catMaybes $ zipWith (fmap . addSemi) tuning $ offsetToRoot fin


arithDists = dists arithmeticDistance

semiDists :: [Pitch] -> [Int]
semiDists = dists semitoneDistance

dists f []     = []
dists f (x:xs) = snd $ mapAccumR (\a y -> (a, f a y)) x (x:xs)

offsetToRoot (Fingering Nothing xs)  = xs
offsetToRoot (Fingering (Just i) xs) = map (root i) xs
  where root i = fmap (\a -> if (a==0) then 0 else a+(i-1))


main = putStr $ hsepS (map affi demo) []
        
demo  = evalFingering c_slash_d standard_tuning
demo2 = evalFingering c_var2 standard_tuning 


c_var2 :: Fingering
c_var2 = decouper "3@(013331)"

c_slash_d :: Fingering
c_slash_d = decouper "(xx0010)"

c_slash_b :: Fingering
c_slash_b = decouper "(x22010)"


f_maj7 :: Fingering
f_maj7 = decouper "(x33210)"


f_maj7s11 :: Fingering
f_maj7s11 = decouper "(1x22xx)"

f_maj7s11' :: Fingering
f_maj7s11' = decouper "(023210)"

f_maj7_a :: Fingering
f_maj7_a = decouper "(x03210)"

g_ :: Fingering
g_ = decouper "(320003)"

g_9 :: Fingering
g_9 = decouper "(3x0201)"

g_9' :: Fingering
g_9' = decouper "(3x32xx)"

f_maj7_c :: Fingering
f_maj7_c = decouper "(x33210)"


c_triad :: Fingering
c_triad = decouper "(x320xx)"

e3s :: Pitch
e3s = decouper "E#3"


--------------------------------------------------------------------------------
-- ascii_fretboard 
--------------------------------------------------------------------------------


-- fn :: Fingering -> FretBoard

data FretBoard = Fretboard {
    top_line        :: Fretting OpenMark,
    fret_start      :: Int,
    fretted_strings :: Fretting FretMark
  }
  

data Fretting a = Fretting a a a a a a

data OpenMark = OO | XX | No

data FretMark = YY | NN



nutLine = replicateS 11 (showChar '=')

fretLine :: ShowS 
fretLine = showChar '+' . replicateS 5 (showString "-+")

markedLine :: (a -> ShowS) -> Fretting a -> ShowS
markedLine fn (Fretting a b c d e f) = 
  fn a `sepS` fn b `sepS` fn c `sepS` fn d `sepS` fn e `sepS` fn f

fingeredLine :: Fretting FretMark -> ShowS
fingeredLine = markedLine fingered

openLine :: Fretting OpenMark -> ShowS
openLine = markedLine open

fingered YY = showChar '@'
fingered NN = showChar '|'

open No = spaceS
open OO = showChar 'O'
open XX = showChar 'X'




-- Fmaj7...
demoo = vsepS [
  openLine (Fretting No XX No No No OO), 
  nutLine, 
  fingeredLine (Fretting YY NN NN NN YY NN), 
  fretLine
  ]


--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Deco Fingering where 
  deco = decoFingering

decoFingering = Fingering <$> optparse fretpos <*> parens (many1 fingerPos)
  where fretpos = int <* char '@'

  
fingerPos = fingered <|> notfingered
  where fingered    = Just <$> digiti
        notfingered = Nothing <$  char 'x'
          

 
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
  
instance Affi Fingering where
  affi (Fingering fret xs) 
      = prefix fret . (parenS $ hcatS $ map affiFingerPos xs)
    where
      prefix Nothing  = id
      prefix (Just i) = shows i . showChar '@' 
    
affiFingerPos Nothing  = showChar 'x'
affiFingerPos (Just i) = shows i

