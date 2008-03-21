--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PitchOps
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A datatypes for representing pitch
-- |
--------------------------------------------------------------------------------

module Bala.Base.PitchOps where

import Bala.Base.PitchRep

import Bala.Base.BaseExtra

-- a 'smart constructor'

buildPitch :: PitchLabel -> Int -> Int -> Pitch
buildPitch lbl o c = Pitch lbl o (semitones lbl) c
  where
    semis C = 0
    semis D = 2
    semis E = 4
    semis F = 5
    semis G = 7
    semis A = 9
    semis B = 11
    
    
-- pitch ops -- adding intervals etc need a naming scheme




class SemiDisplacement a where 
  addSemi :: a -> Int -> a
  subSemi :: a -> Int -> a
  sharp   :: a -> a
  flat    :: a -> a
  
  sharp a = a `addSemi` 1
  flat a  = a `subSemi` 1

class OctaveDisplacement a where   
  addOve  :: a -> Int -> a
  subOve  :: a -> Int -> a
 


spellWithSharps :: PitchLabel -> PitchLabel
spellWithSharps (PitchLabel l a)   = 
  toEnum $ semis l + semis a


instance SemiDisplacement PitchLabel where
  addSemi pl i = toEnum $ (fromEnum pl) + i
  subSemi pl i = toEnum $ (fromEnum pl) - i



instance SemiDisplacement Pitch where
  -- oc is "octave-carry" this isn't a very descriptive implementation
  -- and at some point should be done better
  (Pitch l o s c) `addSemi` i = 
    let (oc,s') = explode12 $ s + i
    in Pitch (l `addSemi` i) (o + oc) s' c
  
  
  (Pitch l o s c) `subSemi` i = 
    let (oc,s') = explode12 $ s - i
    in Pitch (l `subSemi` i) (o - oc) s' c
  
  
    
instance OctaveDisplacement Pitch where   
  (Pitch l o s c) `addOve` i = Pitch l (o + i) s c
  (Pitch l o s c) `subOve` i = Pitch l (o - i) s c
  
centValue :: Pitch -> Int
centValue (Pitch l o s c) 
  = (octaveDisplacement o * 100) + (s * 100) + c

pitchValue :: Int -> Pitch
pitchValue i = error "pitchValue"

{-  
toCents (Pitch o l a c) = Cents $ 
  (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c

fromCents (Cents i) = undefined
-}






class SemiToneCount a where semis :: a -> Int

-- The semitone displacement upwards from C
instance SemiToneCount PitchLetter where
  semis a        = semitones a

-- How many semitones is the pitch changed by its accidental? 
instance SemiToneCount Accidental where
  semis a        = semitones a


instance SemiToneCount Pitch where
  semis (Pitch l o s _) = s + (12 * o)
   
octaveDisplacement oct            = (oct - 4) * 12  
  


  
class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a

instance EncodePitch Int where
  toPitch i = pitchValue i
  fromPitch p = centValue p 

sem :: Pitch -> Int -> Pitch
sem p i = error "sem" -- toPitch $ i + unCents (toCents p)
 
ove :: Pitch -> Int -> Pitch
ove p@(Pitch {octave=o'}) i = p {octave=o'+i} 

data ParsonsCode = PaR | PaU | PaD    
  deriving (Eq,Ord,Show)
  
contour :: [Pitch] -> [ParsonsCode]  
contour = zam diff
  where diff a b = case a `compare` b of
                    EQ -> PaR
                    LT -> PaU
                    GT -> PaD
  
data RefinedContour = ReR | ReUS | ReUL | ReDS | ReDL
  deriving (Eq,Ord,Show)  