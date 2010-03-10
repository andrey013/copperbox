{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Duration
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Duration representation.
--
--------------------------------------------------------------------------------



module Neume.Core.Duration 
  (
  -- * Duration types
    Duration
  
  , DurationMeasure
  
  -- * Classes
  , HasDuration(..)
  , MakeRest(..)
  , MakeSpacer(..)
  
  -- * Operations
  , isZero
  , isDotted
  , dot
  , components
  , extent
  , extentComponents

  , ConversionError(..)
  , rationalToDuration
  
  -- * LilyPond representation
  , LyNumeral(..)
  , lyRepresentation

  -- * ABC representation
  , AbcMultiplier(..)
  , abcRepresentation
  , abcMultiplier

  -- * Named durations
  , dZero
  , longa
  , breve
  , wn
  , hn
  , qn
  , en
  , sn
  , tn
  , dhn
  , dqn
  , den
  , dsn

  , whole_note
  , half_note 
  , quarter_note
  
  ) where

import Text.PrettyPrint.Leijen hiding ( dot )     -- package: wl-pprint 

import Data.Ratio

data Numeral = N128  | N64   | N32   | N16
             | N8    | N4    | N2    | N1
             | Breve | Longa
  deriving (Enum,Eq,Ord)



data Duration = DZero
              | D1 { _dNumeral :: Numeral, _dotCount :: Int }
  deriving Eq

-- | R(ational) Duration supports summation.
type DurationMeasure = Rational

--------------------------------------------------------------------------------
-- Classes



-- is this one still useful... ?
class HasDuration t where
  getDuration  :: t Duration -> Duration

class MakeSpacer e where
  makeSpacer :: Duration -> e

class MakeSpacer e => MakeRest e where
  makeRest :: Duration -> e 


--------------------------------------------------------------------------------
-- Instances

instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2


instance Show Duration where
  show = ('#' :) . show . extent

--------------------------------------------------------------------------------


-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them. 
isZero :: Duration -> Bool
isZero DZero = True
isZero _     = False


isDotted :: Duration -> Bool
isDotted DZero     = False 
isDotted (D1 _ dc) = dc>0

       
-- | Dot a duration. 
-- Note, if the duration represents a concatenation of two or more 
-- primitive durations only the first will be dotted.
dot :: Duration -> Duration
dot DZero     = error "Duration.dot - cannot dot 0 duration"
dot (D1 n dc) = D1 n (dc+1)


components :: Duration -> (Rational,Int)
components DZero        = (0,0)
components (D1 n dc) = (toRat n,dc)

-- | 'extent' - get the size of a Duration as a Rational 
-- (DurationMeasure).
--
extent :: Duration -> DurationMeasure
extent DZero                 = 0 
extent (D1 n dc) | dc <= 0   = toRat n
                 | otherwise = let r = toRat n in step r (r/2) dc
  where
    step acc _ 0 = acc
    step acc h i = step (acc + h) (h/2) (i-1)

-- | 'extentComponents' - get the numerator and denominator of 
-- the duration expressed as a rational.
-- 
extentComponents :: Duration -> (Int,Int)
extentComponents drn = let e = extent drn in 
    (fromIntegral $ numerator e, fromIntegral $ denominator e)

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

newtype ConversionError = ConversionError { getBadRational :: Rational }

instance Show ConversionError where
  showsPrec p = showsPrec p . getBadRational

-- | Convert a rational to a duration - dotting and double dotting
-- is supported.
rationalToDuration :: Rational -> Either ConversionError Duration
rationalToDuration r | r == 4%1   = Right $ D1 Longa 0
                     | r == 2%1   = Right $ D1 Breve 0
                     | r == 0     = Right DZero
                     | r >  1     = cvErr r   -- stops nonsense eg. (7%4)
                     | otherwise  = let (n,d) = (numerator r,denominator r)
                                    in either Left (dotfun n) $ fn d 
  where
    dotfun i sym | i == 1    = Right $ D1 sym 0
                 | i == 3    = Right $ D1 (succ sym) 1
                 | i == 7    = Right $ D1 (succ $ succ sym) 2
                 | otherwise = cvErr r
    fn 1   = Right N1
    fn 2   = Right N2
    fn 4   = Right N4
    fn 8   = Right N8
    fn 16  = Right N16
    fn 32  = Right N32
    fn 64  = Right N64
    fn 128 = Right N128
    fn _   = cvErr r
    
    cvErr  = Left . ConversionError
 

--------------------------------------------------------------------------------
-- LilyPond representation

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

--------------------------------------------------------------------------------
-- ABC representation

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


--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty Duration where
  pretty DZero                 = empty
  pretty (D1 n dc) | dc <= 0   = pretty n
                   | otherwise = pretty n <> (text $ replicate dc '.')

instance Pretty Numeral where
  pretty N128  = int 128
  pretty N64   = int 64
  pretty N32   = int 32
  pretty N16   = int 16
  pretty N8    = int 8 
  pretty N4    = int 4
  pretty N2    = int 2
  pretty N1    = empty
  pretty Breve = text "breve"
  pretty Longa = text "longa"

--------------------------------------------------------------------------------
-- Named durations


dZero :: Duration
dZero = DZero

 
makeDuration :: Numeral -> Duration
makeDuration nm = D1 nm 0


longa           :: Duration
breve           :: Duration
wn              :: Duration
hn              :: Duration
qn              :: Duration
en              :: Duration
sn              :: Duration
tn              :: Duration


longa           = makeDuration Longa
breve           = makeDuration Breve
wn              = makeDuration N1
hn              = makeDuration N2
qn              = makeDuration N4
en              = makeDuration N8
sn              = makeDuration N16
tn              = makeDuration N32

dhn   :: Duration
dqn   :: Duration
den   :: Duration
dsn   :: Duration

dhn   = dot hn
dqn   = dot qn 
den   = dot en 
dsn   = dot sn



whole_note      :: DurationMeasure
half_note       :: DurationMeasure
quarter_note    :: DurationMeasure
whole_note      = 1%1
half_note       = 1%2
quarter_note    = 1%4 
