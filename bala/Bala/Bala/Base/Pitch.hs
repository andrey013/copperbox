{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PitchRep
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention - pulls in the HNotate.Pitch module
--
--------------------------------------------------------------------------------

module Bala.Base.Pitch (
  module HNotate.Pitch,
  
  Semitone, 
  pitch, 
  
  PitchContent(..),  
  
  addSemitone, subSemitone,
  addOctave, subOctave,
  
  Relabel(..),
  
    
  ) where

import Bala.Base.BaseExtra

-- Use the Pitch format defined in HNotate
import HNotate.Pitch





--------------------------------------------------------------------------------
-- Datatypes

-- Pitch & PitchLabel imported from HNotate

type Semitone = Int
  
  
-- Alternative constructor...
pitch :: PitchLabel -> Int -> Pitch
pitch lbl o = Pitch (pch_lbl_letter lbl) (pch_lbl_accidental lbl) o 




--------------------------------------------------------------------------------
-- * Typeclasses for pitched values

-- PitchValue now in HNotate

class PitchContent a where
  pitchContent :: a -> [Pitch]  
  
  



--------------------------------------------------------------------------------
-- Magnitude instances    

instance Increment PitchLetter Int where
  l `increase` i = toEnum $ mod `flip` (7::Int) $ fromEnum l + i
  l `decrease` i = toEnum $ mod `flip` (7::Int) $ fromEnum l - i
  
  
instance Increment PitchLabel Semitone where
  l `increase` i = toEnum $ mod12 $ fromEnum l + i
  l `decrease` i = toEnum $ mod12 $ fromEnum l - i
  
  
instance Increment Pitch Semitone where
  p `increase` i = p + fromIntegral i 
  p `decrease` i = p - fromIntegral i

instance Synonym PitchLabel where
  synonym a b = semitones a == semitones b 
  
instance Synonym Pitch where
  synonym a b = semitones a == semitones b 

--------------------------------------------------------------------------------
-- Instances


    
instance Displacement Pitch Semitone where
  p `displacement` p' = semitones p' - semitones p

  
      


-- | PitchLabels do not generate continuous semitone values ...
-- Displacement should choose the smaller absolute 
-- distance of l to l' or l' to l 
-- C -> B should generate -1 (not 11)

    
instance Displacement PitchLabel Semitone where
  displacement l l' = 
      let s  = semitones l
          s' = if (l' < l) then (12 + semitones l') else semitones l'
          d  = s' - s
     in if (d < 7) then d else negate $ 12 - d  
    
  
instance Ord PitchLabel where
  compare l l' = fromEnum l `compare` fromEnum l'

      
      
--------------------------------------------------------------------------------
-- specific magnitude functions 


-- | Add a semitone to a value that has /semitone magnitude/.
addSemitone :: Increment a Semitone => a -> a
addSemitone a = a `increase` (1::Semitone)

-- | Subtract a semitone from a value that has /semitone magnitude/.
subSemitone :: Increment a Semitone => a -> a
subSemitone a = a `decrease` (1::Semitone)

-- | Add an octave to a value that has /semitone magnitude/.
addOctave  :: Increment a Semitone => a -> a
addOctave a = a `increase` (12::Semitone)

-- | Subtract an octave from a value that has /semitone magnitude/.
subOctave  :: Increment a Semitone => a -> a
subOctave a = a `decrease` (12::Semitone)



  
    
        
--------------------------------------------------------------------------------
-- 

-- pitch ops

-- HNotate has a rather complicated spelling system
-- If we have intervals is it easier? 
    


-- | Spell the 'PitchLabel' according to the 'PitchLetter', changing the 
-- accidental as required, for instance:
--
-- >   relabel (PitchLabel F Sharp) G = PitchLabel G Flat
--
-- If the pitch distance is greater than two semitones, return the original 
-- spelling 

class Relabel a where relabel :: a -> PitchLetter -> a 

instance Relabel PitchLabel where
  relabel lbl l = if (abs dist > 2) then lbl else PitchLabel l (alter dist)
    where
      dist  = displacement (PitchLabel l Nat) lbl  
  
      alter :: Int -> Accidental  
      alter 0     = Nat
      alter (-1)  = Flat
      alter 1     = Sharp
      alter  (-2) = DoubleFlat
      alter 2     = DoubleSharp
      alter _     = error $ "relabel - unreachable"
      
instance Relabel Pitch where
  relabel (Pitch l a o) l' = 
      let lbl   = PitchLabel l a
          lbl'  = relabel lbl l'
      -- must account for the octave 'wraparound' e.g. B#5 -> C6, or Cb6 -> B5     
      in case compare (semitones lbl') (semitones lbl) of
            EQ -> pitch lbl' o
            LT -> pitch lbl' (o+1)
            GT -> pitch lbl' (o-1)
                        




--------------------------------------------------------------------------------
-- Num instances
--------------------------------------------------------------------------------

binop :: (Int -> Int -> Int) -> Pitch -> Pitch -> Pitch
binop op p p' = fromInteger $ fromIntegral $ semitones p `op` semitones p' 

unop :: (Int -> Int) -> Pitch -> Pitch
unop op p = fromInteger $ fromIntegral $ op (semitones p)
 
instance Num Pitch where
  (+) = binop (+)
  (-) = binop (-)
  (*) = binop (*)
  abs = unop abs
  signum = unop signum
  fromInteger i = let (o,en) = (fromIntegral i) `divMod` 12
                  in pitch (toEnum en) o   
  


    
