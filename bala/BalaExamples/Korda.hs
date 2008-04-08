

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
--
--------------------------------------------------------------------------------


module Main where

import Bala

import Bala.Format.Ascii.AsciiGuitar

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

c_maj7 :: Fingering
c_maj7 = decouper "(x32000)"

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

