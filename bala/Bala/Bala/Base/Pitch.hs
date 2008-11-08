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
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Base.Pitch (
  module HNotate.Pitch,
  
  Semitone, 
  pitch, 
  
  -- * Typeclasses
  Pitched(..), EncodePitch(..),
  
  addSemitone, subSemitone,
  addOctave, subOctave,
  
  -- * Operations
  spell,
  semitoneDistance, semitoneDirection,
  
  unaltered,
  


  
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


class (Semitones a) => Pitched a where

    -- | Are two pitched values enharmonically equal? 
    enharmonic            :: a -> a -> Bool
    
    -- | The semitone distance from one element to another, and the direction 
    -- of the distance. 'Upwards' if the second element had a higher pitch than 
    -- the first, otherwise 'Downwards'
    semitoneDisplacement  :: a -> a -> (Direction,Int)
    
    enharmonic a b      = semitones a == semitones b     



-- | Convert to and from the primary pitch representation.
class EncodePitch a where 
    -- | Convert to a Pitch.
    toPitch       :: a -> Pitch 
    -- | Convert from a Pitch. 
    fromPitch     :: Pitch -> a


--------------------------------------------------------------------------------
-- Magnitude instances    
    
instance Magnitude PitchLabel Semitone where
  l `increase` i = toEnum $ mod12 $ fromEnum l + i
  l `decrease` i = toEnum $ mod12 $ fromEnum l - i
  
  
instance Magnitude Pitch Semitone where
  p `increase` i = p + fromIntegral i 
  p `decrease` i = p - fromIntegral i

--------------------------------------------------------------------------------
-- specific magnitude functions 


-- | Add a semitone to a value that has Semitone Magnitude.
addSemitone :: Magnitude a Semitone => a -> a
addSemitone a = a `increase` (1::Semitone)

-- | Subtract a semitone from a value that has Semitone Magnitude.
subSemitone :: Magnitude a Semitone => a -> a
subSemitone a = a `decrease` (1::Semitone)

-- | Add an octave to a value that has Semitone Magnitude.
addOctave  :: Magnitude a Semitone => a -> a
addOctave a = a `increase` (12::Semitone)

-- | Subtract an octave from a value that has Semitone Magnitude.
subOctave  :: Magnitude a Semitone => a -> a
subOctave a = a `decrease` (12::Semitone)



  
    
        
--------------------------------------------------------------------------------
-- 

-- pitch ops -- adding intervals etc need a naming scheme

-- | Spell the 'PitchLabel' according to the 'PitchLetter', changing the 
-- accidental as required, for instance:
--
-- >   spell (PitchLabel F (Sharp 1)) G = (PitchLabel G (Flat 1))
--
-- If the pitch distance is greater than two semitones, return the original 
-- spelling 
spell :: PitchLabel -> PitchLetter -> PitchLabel
spell lbl l' = if (abs dist > 2) then lbl else PitchLabel l' (alter dist)
  where
    (dr,d) = semitoneDisplacement lbl (PitchLabel l' Nat)
    dist   = case dr of Upwards -> negate d; _ -> d 

    alter :: Int -> Accidental
    alter 0     = Nat
    alter (-1)  = Flat
    alter 1     = Sharp
    alter  (-2) = DoubleFlat
    alter 2     = DoubleSharp
    alter z     = error $ "alter " ++ show z


spellWithSharps :: PitchLabel -> PitchLabel
spellWithSharps lbl   = 
  toEnum $ semitones lbl


unaltered :: PitchLabel -> Bool  
unaltered lbl = pch_lbl_accidental lbl == Nat

octaveDisplacement oct            = (oct - 4) * 12  
  




-- | As per 'semitoneDisplacement' but just return the distance.
semitoneDistance    :: Pitched a => a -> a -> Int
semitoneDistance    = snd `dyap` semitoneDisplacement

-- | As per 'semitoneDisplacement' but just return the direction.   
semitoneDirection   :: Pitched a => a -> a -> Direction
semitoneDirection   = fst `dyap` semitoneDisplacement  



--------------------------------------------------------------------------------
-- Instances


    
instance Pitched Pitch where
  p `semitoneDisplacement` p' = let d = semitones p' - semitones p
                                in case signum d of
                                    (-1) -> (Downwards, abs d)
                                    _    -> (Upwards, d)
  
      
{-
-- C-nat = 0 
instance Semitones PitchLetter where
  semitones l = fromEnum (PitchLabel l Nat)

instance Semitones Accidental where
  semitones Nat          = 0
  semitones Sharp        = 1
  semitones DoubleSharp  = 2
  semitones Flat         = (-1)
  semitones DoubleFlat   = (-2)

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
-}

-- | The names generated by toEnum favour sharps, pitches may need re-spelling.     
instance Pitched PitchLabel where
  semitoneDisplacement l l' =
    let d = semitones l - semitones l'
    in if (d > 6) then (Downwards, 12 - d) else (Upwards, abs d)

      

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
  


    
