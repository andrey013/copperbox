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



class HasDuration a where
  getDuration  :: a -> Duration

instance HasDuration Duration where
  getDuration  = id


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



lilypond :: Duration -> Maybe (Either String Int,Int)
lilypond DZero     = Nothing
lilypond (D1 n dc) = Just (numeralLy n, dc)

numeralLy :: Numeral -> Either String Int
numeralLy N128  = Right 128
numeralLy N64   = Right 64
numeralLy N32   = Right 32
numeralLy N16   = Right 16
numeralLy N8    = Right 8
numeralLy N4    = Right 4
numeralLy N2    = Right 2
numeralLy N1    = Right 1
numeralLy Breve = Left "breve"
numeralLy Longa = Left "longa"

data AbcMultiplier = IdenM | Mult Integer | Div Integer | Frac Integer Integer
  deriving (Eq,Show)

abc :: Rational -> Duration -> Maybe AbcMultiplier
abc _   DZero     = Nothing
abc unl d         = Just $ multiplierABC unl d

multiplierABC :: Rational -> Duration -> AbcMultiplier
multiplierABC unl nd = (fn . fork numerator denominator) $ (extent nd) / unl
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



