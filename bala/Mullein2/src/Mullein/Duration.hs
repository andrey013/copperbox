{-# LANGUAGE KindSignatures             #-}
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


import Data.Ratio


data Numeral = N128  | N64   | N32   | N16
             | N8    | N4    | N2    | N1
             | Breve | Longa
  deriving (Eq,Ord)



data Duration = DZero
              | D1 { dNumeral :: Numeral, dotCount :: Int }
  deriving Eq


instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2


instance Show Duration where
  show = ('#' :) . show . extent



class HasDuration (t :: * -> *) where
  getDuration  :: t Duration -> Duration




class Spacer a where
  spacer :: Duration -> a

toRat :: Numeral -> Rational
toRat N128  = 1%128
toRat N64   = 1%64
toRat N32   = 1%32
toRat N16   = 1%16
toRat N8    = 1%8
toRat N4    = 1%4
toRat N2    = 1%2
toRat N1    = 1
toRat Breve = 2
toRat Longa = 4

-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them. 
isZero :: Duration -> Bool
isZero DZero = True
isZero _     = False

dZero :: Duration
dZero = DZero


isDotted :: Duration -> Bool
isDotted DZero     = False 
isDotted (D1 _ dc) = dc>0


components :: Duration -> (Rational,Int)
components DZero        = (0,0)
components (D1 n dc) = (toRat n,dc)

-- | @extent@ is the sum of all symbolic components of a duration.
extent :: Duration -> Rational
extent DZero                 = 0 
extent (D1 n dc) | dc <= 0   = toRat n
                 | otherwise = let r = toRat n in step r (r/2) dc
  where
    step acc _ 0 = acc
    step acc h i = step (acc + h) (h/2) (i-1)


       
-- | Dot a duration. 
-- Note, if the duration represents a concatenation of two or more 
-- primitive durations only the first will be dotted.
dot :: Duration -> Duration
dot DZero     = error "Duration.dot - cannot dot 0 duration"
dot (D1 n dc) = D1 n (dc+1)

data LyNumeral = LyCmd String
               | LyNum Int

lyRepresentation :: Duration -> Maybe (LyNumeral,Int)
lyRepresentation DZero     = Nothing
lyRepresentation (D1 n dc) = Just (lyNumeral n, dc)

lyNumeral :: Numeral -> LyNumeral
lyNumeral N128  = LyNum 128
lyNumeral N64   = LyNum 64
lyNumeral N32   = LyNum 32
lyNumeral N16   = LyNum 16
lyNumeral N8    = LyNum 8
lyNumeral N4    = LyNum 4
lyNumeral N2    = LyNum 2
lyNumeral N1    = LyNum 1
lyNumeral Breve = LyCmd "breve"
lyNumeral Longa = LyCmd "longa"

data AbcMultiplier = IdenM | Mult Integer | Div Integer | Frac Integer Integer
  deriving (Eq,Show)

abcRepresentation :: Rational -> Duration -> Maybe AbcMultiplier
abcRepresentation _   DZero     = Nothing
abcRepresentation unl d         = Just $ abcMultiplier unl d

abcMultiplier :: Rational -> Duration -> AbcMultiplier
abcMultiplier unl nd = (fn . fork numerator denominator) $ (extent nd) / unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = IdenM
    fn (1,dn)  = Div dn
    fn (nm,1)  = Mult nm
    fn (nm,dn) = Frac nm dn

 
mkDuration :: Numeral -> Duration
mkDuration nm = D1 nm 0


longa :: Duration
breve :: Duration
wn    :: Duration
hn    :: Duration
qn    :: Duration
en    :: Duration
sn    :: Duration
tn    :: Duration


longa = mkDuration Longa
breve = mkDuration Breve
wn    = mkDuration N1
hn    = mkDuration N2
qn    = mkDuration N4
en    = mkDuration N8
sn    = mkDuration N16
tn    = mkDuration N32

dhn   :: Duration
dqn   :: Duration
den   :: Duration
dsn   :: Duration

dhn   = dot hn
dqn   = dot qn 
den   = dot en 
dsn   = dot sn



