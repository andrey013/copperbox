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
  , MakeRest(..)
  , MakeSpacer(..)
  
  -- * Operations
  , isZero
  , isDotted
  , notDotted
  , dot
  , components
  , extent
  , extentComponents
  
  -- * LilyPond representation
  , LyNumeral(..)
  , lyRepresentation

  -- * ABC representation
  , AbcMultiplier(..)
  , abcRepresentation
  , abcMultiplier

  -- * Named durations
  , dZero
  , dLonga
  , dBreve
  , dWhole
  , dHalf
  , dQuarter
  , dEighth
  , dSixteenth
  , dThirtySecondth

  , dmWhole
  , dmHalf 
  , dmQuarter
  
  ) where

import Neume.Core.Utils.Pretty

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

-- class ShowsDuration a where
--  showsDur :: a -> ShowS


class MakeSpacer e where
  makeSpacer :: Duration -> e

class MakeSpacer e => MakeRest e where
  makeRest :: Duration -> e 


--------------------------------------------------------------------------------
-- Instances

instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2

{-
instance Show Duration where
  showsPrec _ drn = shows n . showChar '%' . shows d
                    where (n,d) = extentComponents drn
-}

{-
instance ShowsDuration Duration where
  showsDur = shows

instance Show a => ShowsDuration (Maybe a) where
  showsDur = maybe id shows
-}

--------------------------------------------------------------------------------


-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them. 
isZero :: Duration -> Bool
isZero DZero = True
isZero _     = False


isDotted :: Duration -> Bool
isDotted DZero     = False 
isDotted (D1 _ dc) = dc>0

-- more convenient to have this one...
notDotted :: Duration -> Bool
notDotted = not . isDotted
       
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
-- Show instances

instance Show Duration where
  show DZero                 = ""
  show (D1 n dc) | dc <= 0   = show n
                 | otherwise = docSixty $ dshow n <> (text $ replicate dc '.')

instance Show Numeral where
  show N128  = "128"
  show N64   = "64"
  show N32   = "32"
  show N16   = "16"
  show N8    = "8" 
  show N4    = "4"
  show N2    = "2"
  show N1    = ""
  show Breve = "breve"
  show Longa = "longa"

--------------------------------------------------------------------------------
-- Named durations


dZero :: Duration
dZero = DZero

 
makeDuration :: Numeral -> Duration
makeDuration nm = D1 nm 0


dLonga          :: Duration
dLonga          = makeDuration Longa

dBreve          :: Duration
dBreve          = makeDuration Breve

dWhole          :: Duration
dWhole          = makeDuration N1

dHalf           :: Duration
dHalf           = makeDuration N2

dQuarter        :: Duration
dQuarter        = makeDuration N4

dEighth         :: Duration
dEighth         = makeDuration N8

dSixteenth      :: Duration
dSixteenth      = makeDuration N16

dThirtySecondth :: Duration
dThirtySecondth = makeDuration N32




dmWhole     :: DurationMeasure
dmWhole     = 1%1

dmHalf      :: DurationMeasure
dmHalf      = 1%2

dmQuarter   :: DurationMeasure
dmQuarter   = 1%4 
