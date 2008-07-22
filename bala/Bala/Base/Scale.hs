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

import Bala.Base.Pitch
import Bala.Base.Interval
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

data Key = Key {
    key_pitch :: PitchName,
    key_type  :: KeyType
  }
  deriving (Eq,Show)

data KeyType = MajorKey | MinorKey
  deriving (Eq,Show)  
  
unKey :: Key -> (PitchName,KeyType)
unKey (Key p t) = (p,t)

scaleNotes :: Scale -> [Pitch]
scaleNotes = scale_notes



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

    
      
--------------------------------------------------------------------------------
-- Named elements



c_major             :: Key
c_major             = Key c_natural MajorKey

g_major             :: Key
g_major             = Key g_natural MajorKey

d_major             :: Key
d_major             = Key d_natural MajorKey

a_major             :: Key
a_major             = Key a_natural MajorKey

e_major             :: Key
e_major             = Key e_natural MajorKey

b_major             :: Key
b_major             = Key b_natural MajorKey

f_sharp_major       :: Key
f_sharp_major       = Key f_sharp MajorKey

c_sharp_major       :: Key
c_sharp_major       = Key c_sharp MajorKey

-- Major scales with flats
f_major             :: Key
f_major             = Key f_natural MajorKey

b_flat_major        :: Key
b_flat_major        = Key b_flat MajorKey

e_flat_major        :: Key
e_flat_major        = Key e_flat MajorKey

a_flat_major        :: Key
a_flat_major        = Key a_flat MajorKey

d_flat_major        :: Key
d_flat_major        = Key d_flat MajorKey

g_flat_major        :: Key
g_flat_major        = Key g_flat MajorKey

c_flat_major        :: Key
c_flat_major        = Key (subSemi c_natural 1) MajorKey



-- Minor scales with sharps
a_minor             :: Key
a_minor             = Key a_natural MinorKey

e_minor             :: Key
e_minor             = Key e_natural MinorKey

b_minor             :: Key
b_minor             = Key b_natural MinorKey

f_sharp_minor       :: Key 
f_sharp_minor       = Key f_sharp MinorKey

c_sharp_minor       :: Key 
c_sharp_minor       = Key c_sharp MinorKey

g_sharp_minor       :: Key 
g_sharp_minor       = Key g_sharp MinorKey

d_sharp_minor       :: Key 
d_sharp_minor       = Key d_sharp MinorKey

a_sharp_minor       :: Key 
a_sharp_minor       = Key a_sharp MinorKey

-- Minor scales with flats
d_minor             :: Key
d_minor             = Key d_natural MinorKey

g_minor             :: Key
g_minor             = Key g_natural MinorKey

c_minor             :: Key
c_minor             = Key c_natural MinorKey

f_minor             :: Key
f_minor             = Key f_natural MinorKey

b_flat_minor        :: Key
b_flat_minor        = Key b_flat MinorKey

e_flat_minor        :: Key
e_flat_minor        = Key e_flat MinorKey

a_flat_minor        :: Key
a_flat_minor        = Key a_flat MinorKey


                   