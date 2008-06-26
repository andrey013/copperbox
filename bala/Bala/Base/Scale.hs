{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Scale
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Scale representation
--
--------------------------------------------------------------------------------

module Bala.Base.Scale where

import Bala.Base.PitchRep
import Bala.Base.Interval
import Bala.Base.PitchOps
import Bala.Base.BaseExtra


import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

data Scale = Scale {
    scale_root  :: Pitch,
    scale_notes :: [Pitch]
  }
  deriving (Show)
  
data ScaleDegree = Tonic | SuperTonic | Mediant | Subdominant | Dominant
                 | Submediant | LeadingTone 
  deriving (Eq,Show)



instance Extract Scale [Pitch] where
  extract = scale_notes



-- Interval patterns
mkIS :: String -> IntervalPattern
mkIS = decouper

-- | Does the interval pattern generate a one octave scale?
octaveComplete :: IntervalPattern -> Bool
octaveComplete (IntervalPattern xs) = 12 == foldr fn 0 xs
  where fn ival n = n + halfSteps ival



makeScale :: Pitch -> IntervalPattern -> Scale
makeScale p (IntervalPattern xs) = Scale p $ scanl extUp p xs

makeDescendingScale :: Pitch -> IntervalPattern -> Scale
makeDescendingScale p (IntervalPattern xs) = Scale p $ scanl extDown p xs
 
--------------------------------------------------------------------------------
-- Deco instances
--------------------------------------------------------------------------------




         
--------------------------------------------------------------------------------
-- Affi instances
--------------------------------------------------------------------------------
instance Affi Scale where
  affi (Scale r ps) = hsepS $ map affi ps

    
      

                   