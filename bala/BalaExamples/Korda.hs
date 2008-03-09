

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


module Korda where

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
        lower_e = read "E3"

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


        
demo  = evalFingering c_slash_d standard_tuning
demo2 = evalFingering c_var2 standard_tuning 


c_var2 :: Fingering
c_var2 = read "3@(013331)"

c_slash_d :: Fingering
c_slash_d = read "(xx0010)"

c_slash_b :: Fingering
c_slash_b = read "(x22010)"


f_maj7 :: Fingering
f_maj7 = read "(x33210)"


f_maj7s11 :: Fingering
f_maj7s11 = read "(1x22xx)"

f_maj7s11' :: Fingering
f_maj7s11' = read "(023210)"

f_maj7_a :: Fingering
f_maj7_a = read "(x03210)"

g_ :: Fingering
g_ = read "(320003)"

g_9 :: Fingering
g_9 = read "(3x0201)"

g_9' :: Fingering
g_9' = read "(3x32xx)"

f_maj7_c :: Fingering
f_maj7_c = read "(x33210)"


c_triad :: Fingering
c_triad = read "(x320xx)"

e3s :: Pitch
e3s = read "E#3"


--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read Fingering where 
  readsPrec _ s = readsParsec fingering s

fingering = Fingering <$> optparse fretpos <*> parens (many1 fingerPos)
  where fretpos = int <* char '@'

  
fingerPos = fingered <|> notfingered
  where fingered    = Just <$> digiti
        notfingered = Nothing <$  char 'x'
          

 
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
  
instance Show Fingering where
  showsPrec _ (Fingering fret xs) 
      = prefix fret . (withParens $ caten $ map showsFingerPos xs)
    where
      prefix Nothing  = id
      prefix (Just i) = shows i . showChar '@' 
    
showsFingerPos Nothing  = showChar 'x'
showsFingerPos (Just i) = shows i

