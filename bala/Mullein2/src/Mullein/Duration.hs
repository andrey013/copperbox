{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Duration
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Duration representation.
--
--------------------------------------------------------------------------------



module Mullein.Duration where

import Data.OneMany

import Data.Monoid
import Data.Ratio

-- rational duration x dot count
newtype D1 = D1 { getComponent :: (Rational,Int) }
 deriving Eq

instance Ord D1 where
  compare d1 d2 = sumD1 d1 `compare` sumD1 d2


data Duration = DZero 
              | Dn (OneMany D1)
  deriving Eq


instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2


instance Show Duration where
  show = ('#' :) . show . extent



instance Monoid Duration where
  mempty  = DZero
  mappend = (#+)

{-
  a     `mappend` DZero = a
  DZero `mappend` b     = b
  a     `mappend
                | isZero b  = a
                | otherwise = a #+ b   
-}

class HasDuration a where
  getDuration  :: a -> Duration
  setDuration  :: Duration -> a -> a

instance HasDuration Duration where
  getDuration  = id
  setDuration  = const


class Spacer a where
  spacer :: Duration -> a



-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them. 
isZero :: Duration -> Bool
isZero DZero = True
isZero _    = False

dZero :: Duration
dZero = DZero

-- | Composite durations have more than 1 component typically they
-- will be represented as tied notes.
isComposite :: Duration -> Bool
isComposite DZero   = False
isComposite (Dn om) = isOne om 



components :: Duration -> [(Rational,Int)]
components DZero   = []
components (Dn om) = oneMany (wrap . getComponent) (map getComponent) om 
  where wrap a = [a]
   


-- | @extent@ is the sum of all symbolic components of a duration.
extent :: Duration -> Rational
extent DZero = 0 
extent (Dn xs) = oneMany sumD1 (foldr (\a s -> s + sumD1 a) 0) xs



sumD1 :: D1 -> Rational
sumD1 (D1 (r,i)) | i <= 0 = r
sumD1 (D1 (r,i))          = step r (r/2) i where
  step acc _ 0 = acc
  step acc h n = step (acc + h) (h/2) (n-1)


-- | Add (concatenate) two durations. 
-- Addition (concatenation) produces a symbolic value equivalent 
-- to the two durations being tied: @1/4 #+ 1/4 = 1/4~1/4@.
-- It does not perform numeric addition: @1/4 #+ 1/4 /= 1/2@.
(#+) :: Duration -> Duration -> Duration
DZero  #+ b      = b
a      #+ DZero  = a
Dn oml #+ Dn omr = Dn $ foldr cons omr (toList oml)

       
-- | Dot a duration. 
-- Note, if the duration represents a concatenation of two or more 
-- primitive durations only the first will be dotted.
dot :: Duration -> Duration
dot DZero = error "Duration.dot - cannot dot 0 duration"
dot (Dn om) = Dn $ oneMany (one . dot1) (many . first dot1) om
  where
    dot1 (D1 (a,n))   = D1 (a,n+1)
    first f (x:xs)  = f x:xs
    first _ []      = error $ "Duration.dot - bad list"  -- should be unreachable in this case
  


{-
best :: Rational -> Int ->  Duration
best _ i | i < 1     = error "Duration.best - iteration counter must be >= 1"
best r i | r >= 1    = stepUp wn (r-1) (i-1)
         | otherwise = undefined 
  where
    stepUp ac _ 0    = ac
    stepUp ac v n    | v < 1     = ac #+ (stepDown v n)
                     | otherwise = stepUp (ac #+ wn) (v-1) (n-1)

    stepDown _ _     = undefined  -- remember stepDown "is" (/2)
-}



mkDuration :: Rational -> Duration
mkDuration r = Dn $ one $ D1 (r,0)


longa :: Duration
breve :: Duration
wn    :: Duration
hn    :: Duration
qn    :: Duration
en    :: Duration
sn    :: Duration
tn    :: Duration


longa = mkDuration 4
breve = mkDuration 2
wn    = mkDuration 1
hn    = mkDuration (1%2)
qn    = mkDuration (1%4)
en    = mkDuration (1%8)
sn    = mkDuration (1%16)
tn    = mkDuration (1%32)

dhn :: Duration
dqn :: Duration
den :: Duration
dsn :: Duration

dhn = dot hn
dqn = dot qn 
den = dot en 
dsn = dot sn



