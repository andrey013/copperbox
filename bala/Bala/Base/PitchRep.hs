

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
-- A datatypes for representing pitch
-- |
--------------------------------------------------------------------------------

module Bala.Base.PitchRep where

import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- Cache the pitch letter & accidental for spelling only, and store semitones 
-- for calculations  
data Pitch = Pitch {
    label     :: PitchLabel,
    octave    :: Int,
    seimtones :: Int,
    cents     :: Int 
  }
  deriving Eq 
  
data PitchLabel = PitchLabel PitchLetter Accidental
  deriving Eq 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)

data Accidental = Nat | Sharpi Int | Flati Int
  deriving Eq
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

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
  addSemi (PitchLabel l a) i = toEnum $ semis l + semis a + i
  subSemi (PitchLabel l a) i = toEnum $ semis l + semis a - i



instance SemiDisplacement Pitch where
  -- oc is "octave-carry" this isn't a very descriptive implementation
  -- and at some point should be done better
  (Pitch l o s c) `addSemi` i = 
    let od = i `div` 12
        -- (PitchLabel l' a') = toEnum $ semis l + semis a + (i `mod` 12)
        oc = 0 -- if (l' < l) then 1 else 0
    in Pitch l (o + od + oc) (s + i) c
  
  
  (Pitch l o s c) `subSemi` i = 
    let od = i `div` 12 
        -- (PitchLabel l' a') = toEnum $ semis l + semis a - (i `mod` 12)
        oc = 0 -- if (l' > l) then 1 else 0
    in Pitch l (o - (od + oc)) (s - i) c
  
  
    
instance OctaveDisplacement Pitch where   
  (Pitch l o s c) `addOve` i = Pitch l (o + i) s c
  (Pitch l o s c) `subOve` i = Pitch l (o - i) s c
  
centValue :: Pitch -> Int
centValue (Pitch l o s c) 
  = (octaveDisplacement o * 100) + (s * 100) + c

pitchValue :: Int -> Pitch
pitchValue i = undefined

{-  
toCents (Pitch o l a c) = Cents $ 
  (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c

fromCents (Cents i) = undefined
-}


class Semitones a where semitones :: a -> Int

-- The semitone displacement upwards from C
instance Semitones PitchLetter where
  semitones C = 0
  semitones D = 2
  semitones E = 4
  semitones F = 5
  semitones G = 7
  semitones A = 9
  semitones B = 11

-- How many semitones is the pitch changed by its accidental? 
instance Semitones Accidental where
  semitones Nat        = 0
  semitones (Sharpi i) = i
  semitones (Flati i)  = negate i

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
    
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
sem p i = undefined -- toPitch $ i + unCents (toCents p)
 
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

--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Ord Pitch where
  p1 `compare` p2 = undefined -- (toCents p1) `compare` (toCents p2)



--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read Pitch where 
  readsPrec _ s = readsParsec readPitch s

readPitch :: Parser Pitch
readPitch = buildPitch <$> readPitchLabel
                       <*> option 4 positiveInt 
                       <*> option 0 signedInt
                



instance Read PitchLabel where 
  readsPrec _ s = readsParsec readPitchLabel s
  

readPitchLabel = PitchLabel <$> readPitchLetter <*> readAccidental
                
instance Read PitchLetter where 
  readsPrec _ s = readsParsec readPitchLetter s
  
readPitchLetter = letter <$> oneOf "ABCDEFG" 
  where 
    letter 'A' = A
    letter 'B' = B
    letter 'C' = C
    letter 'D' = D
    letter 'E' = E
    letter 'F' = F
    letter 'G' = G


instance Read Accidental where 
  readsPrec _ s = readsParsec readAccidental s
  
readAccidental = accident <$> option "" sf
  where 
    sf = many1 (char '#') <|> many1 (char 'b')
    accident ""       = Nat
    accident ('#':xs) = Sharpi (1 + length xs)
    accident ('b':xs) = Flati (1 + length xs)
    
      
    
instance Show Accidental where
  showsPrec _ Nat         = showString ""
  showsPrec _ (Sharpi i)  | mod i 2 == 0  = root 
                          | otherwise     = root . showChar '#'
    where root = showString (replicate (i `div` 2) 'x')                          
  showsPrec _ (Flati i)   = showString (replicate i 'b')
    
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show Pitch where
  showsPrec _ (Pitch o p a i) | i == 0    = root
                              | i < 0     = root . shows i
                              | otherwise = root . showChar '+' . shows i
    where root = shows p . shows a . shows o



instance Show PitchLabel where
  showsPrec _ (PitchLabel p a) = shows p . shows a

--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum PitchLabel where 
  fromEnum = fromEnumPitchLabel . spellWithSharps
    where
      fromEnumPitchLabel (PitchLabel C Nat)   = 0
      fromEnumPitchLabel (PitchLabel C (Sharpi 1)) = 1
      fromEnumPitchLabel (PitchLabel D Nat)   = 2
      fromEnumPitchLabel (PitchLabel D (Sharpi 1)) = 3
      fromEnumPitchLabel (PitchLabel E Nat)   = 4
      fromEnumPitchLabel (PitchLabel F Nat)   = 5
      fromEnumPitchLabel (PitchLabel F (Sharpi 1)) = 6
      fromEnumPitchLabel (PitchLabel G Nat)   = 7
      fromEnumPitchLabel (PitchLabel G (Sharpi 1)) = 8
      fromEnumPitchLabel (PitchLabel A Nat)   = 9
      fromEnumPitchLabel (PitchLabel A (Sharpi 1)) = 10
      fromEnumPitchLabel (PitchLabel B Nat)   = 11
  
  toEnum 0   = PitchLabel C Nat
  toEnum 1   = PitchLabel C (Sharpi 1)
  toEnum 2   = PitchLabel D Nat
  toEnum 3   = PitchLabel D (Sharpi 1)
  toEnum 4   = PitchLabel E Nat
  toEnum 5   = PitchLabel F Nat
  toEnum 6   = PitchLabel F (Sharpi 1)
  toEnum 7   = PitchLabel G Nat
  toEnum 8   = PitchLabel G (Sharpi 1)
  toEnum 9   = PitchLabel A Nat
  toEnum 10  = PitchLabel A (Sharpi 1)
  toEnum 11  = PitchLabel B Nat

  toEnum i  = toEnum $ i `mod` 12
  
  