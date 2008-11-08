

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representations
--
--------------------------------------------------------------------------------


module Bala.Base.Chord where

import Bala.Base.Pitch
import Bala.Base.Interval
import Bala.Base.PitchClass
import Bala.Base.BaseExtra


import qualified Data.Map as Map

data Chord = Chord { 
    chord_root        :: Pitch,
    chord_inversion   :: Inversion,
    chord_elems       :: IntervalMap
  }
  deriving (Show)

data Inversion = RootPosition | FirstInversion | SecondInversion 
               | ThirdInversion | NthInversion Int
  deriving (Eq,Read,Show)


   
type IntervalMap = Map.Map Int Interval 

  
    
                
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

-- | Can't handle inversions, yet
extractNotes :: Chord -> [Pitch]
extractNotes (Chord p i m)  = 
  let pchs = map (increase p) $ map snd (Map.toAscList m)
  {- NO!!! only works if you have a proper sequence c.f. <1,3,5> -}
  in inversion i pchs 
      


inversion :: Inversion -> [Pitch] -> [Pitch]
inversion RootPosition   xs       = xs
inversion i              (x:xs)   = inversion (pred i) (xs ++ [addOctave x])
inversion _              []       = []       

{-
majorTriad :: Pitch -> Chord
majorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, major_third, perfect_fifth]

minorTriad :: Pitch -> Chord
minorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, minor_third, perfect_fifth]
  


buildMap :: [Interval] -> IntervalMap
buildMap = Map.fromAscList . map fn
  where
    fn ivl = (intervalType ivl, ivl)
-}

invert :: Chord -> Chord
invert (Chord p i m) = Chord p (succ i) m

{-
-- | replace or add
roa :: Interval -> Chord -> Chord
roa ivl (Chord p i m) = 
  let idx = intervalType ivl in Chord p i (Map.insert idx ivl m)

del :: Int -> Chord -> Chord
del idx (Chord p i m) = Chord p i (Map.delete idx m)

dim i = roa $ interval Diminished (intervalSize i) 
aug i = roa $ interval Augmented (intervalSize i)

-- | A chord without a root @\<C4 E4 G4\>@ would be @\<E4 G4\>@.
noRoot    :: Chord -> Chord
noRoot    = del 1

-- | A chord without a third @\<C4 E4 G4\>@ would be @\<C4 G4\>@.
no3       :: Chord -> Chord
no3       = del 3

-- | A chord without a fifth @\<C4 E4 G4\>@ would be @\<C4 E4\>@.
no5       :: Chord -> Chord
no5     = del 5

-- | A chord without a seventh.
no7       :: Chord -> Chord
no7     = del 7

-- | A chord without a nineth.
no9       :: Chord -> Chord
no9     = del 9

-- | A chord without a eleventh.
no11      :: Chord -> Chord
no11    = del 11

-- | A chord without a thirteenth.
no13      :: Chord -> Chord
no13    = del 13

dim5 = dim 5
aug5 = aug 5

min7 = roa minor_seventh
maj7 = roa major_seventh
dim7 = dim 7

-}

-- Csus2 <C D G> and Csus4 <C F G>


instance Enum Inversion where 
  fromEnum RootPosition     = 0
  fromEnum FirstInversion   = 1
  fromEnum SecondInversion  = 2
  fromEnum ThirdInversion   = 3 
  fromEnum (NthInversion i) = i
  
  toEnum i | i == 0     = RootPosition
           | i == 1     = FirstInversion
           | i == 2     = SecondInversion
           | i == 3     = ThirdInversion 
           | i >= 4     = NthInversion i
      
               
               
