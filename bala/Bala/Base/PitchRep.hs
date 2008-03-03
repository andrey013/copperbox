
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

data Pitch = Pitch { 
    pitch       :: PitchLetter,
    accidental  :: Accidental,
    octave      :: Int,
    cents       :: Int 
  }
  deriving Eq
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)

data Accidental = Nat | Sharp | SharpSharp | Flat | FlatFlat  
                | Sharpi Int | Flati Int
  deriving Eq

instance Ord Pitch where
  p1 `compare` p2 = (toCents p1) `compare` (toCents p2)

newtype Cents = Cents {unCents :: Int}
  deriving (Eq,Ord,Show)

-- a simple representation (for typography ?)
data SimplePitch = SimplePitch PitchLetter Accidental
  
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
 


spellWithSharps :: SimplePitch -> SimplePitch
spellWithSharps (SimplePitch l a)   = 
  toEnum $ semis l + semis a


instance SemiDisplacement SimplePitch where
  addSemi (SimplePitch l a) i = toEnum $ semis l + semis a + i
  subSemi (SimplePitch l a) i = toEnum $ semis l + semis a - i



instance SemiDisplacement Pitch where
  -- oc is "octave-carry" this isn't a very descriptive implementation
  -- and at some point should be done better
  (Pitch l a o c) `addSemi` i = 
    let od = i `div` 12
        (SimplePitch l' a') = toEnum $ semis l + semis a + (i `mod` 12)
        oc = if (l' < l) then 1 else 0
    in Pitch l' a' (o + od + oc) c
  
  
  (Pitch l a o c) `subSemi` i = 
    let od = i `div` 12 
        (SimplePitch l' a') = toEnum $ semis l + semis a - (i `mod` 12)
        oc = if (l' > l) then 1 else 0
    in Pitch l' a' (o - (od + oc)) c
  
  
    
instance OctaveDisplacement Pitch where   
  (Pitch l a o c) `addOve` i = Pitch l a (o + i) c
  (Pitch l a o c) `subOve` i = Pitch l a (o - i) c
  
centValue :: Pitch -> Int
centValue (Pitch l a o c) 
  = (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c

pitchValue :: Int -> Pitch
pitchValue i = Pitch C Nat 0 0
  
toCents (Pitch l a o c) = Cents $ 
  (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c

fromCents (Cents i) = undefined

class SemiToneCount a where semis :: a -> Int

-- The semitone displacement upwards from C
instance SemiToneCount PitchLetter where
  semis C = 0
  semis D = 2
  semis E = 4
  semis F = 5
  semis G = 7
  semis A = 9
  semis B = 11

-- How many semitones is the pitch changed by its accidental? 
instance SemiToneCount Accidental where
  semis Nat        = 0
  semis Sharp      = 1
  semis SharpSharp = 2
  semis Flat       = (-1)
  semis FlatFlat   = (-2)
  semis (Sharpi i) = i
  semis (Flati i)  = 0 - i

instance SemiToneCount Pitch where
  semis (Pitch p a o _) = semis p + semis a + (12 * o)
   
octaveDisplacement oct            = (oct - 4) * 12  
  


  
class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a

instance EncodePitch Int where
  toPitch i = pitchValue i
  fromPitch p = centValue p 

sem :: Pitch -> Int -> Pitch
sem p i = toPitch $ i + unCents (toCents p)
 
ove :: Pitch -> Int -> Pitch
ove p@(Pitch {octave=o'}) i = p {octave=o'+i} -- (\s -> s {octave=o'+i}) p

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

instance Read Pitch where 
  readsPrec _ s = readsParsec readPitch s

readPitch :: Parser Pitch
readPitch = Pitch <$> readPitchLetter 
                  <*> readAccidental
                  <*> option 4 positiveInt 
                  <*> option 0 signedInt

                
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
    accident "#"      = Sharp
    accident "##"     = SharpSharp
    accident "b"      = Flat
    accident "bb"     = FlatFlat
    accident ('#':xs) = Sharpi (1+ length xs)
    accident ('b':xs) = Flati (1+ length xs)
    
instance Read SimplePitch where
  readsPrec _ s = readsParsec readSimplePitch s
  
readSimplePitch = SimplePitch <$> readPitchLetter <*> readAccidental
    
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show Pitch where
  showsPrec _ (Pitch p a o i) | i == 0    = root
                              | i < 0     = root . shows i
                              | otherwise = root . showChar '+' . shows i
    where root = shows p . shows a . shows o
    
instance Show Accidental where
  showsPrec _ Nat         = showString ""
  showsPrec _ Sharp       = showChar '#'
  showsPrec _ SharpSharp  = showChar 'x'
  showsPrec _ Flat        = showChar 'b'
  showsPrec _ FlatFlat    = showString "bb"
  showsPrec _ (Sharpi i)  | mod i 2 == 0  = root 
                          | otherwise     = root . showChar '#'
    where root = showString (replicate (i `div` 2) 'x')                          
  showsPrec _ (Flati i)   = showString (replicate i 'b')


instance Show SimplePitch where
  showsPrec _ (SimplePitch p a) = shows p . shows a

--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum SimplePitch where 
  fromEnum = fromEnumSimple . spellWithSharps
    where
      fromEnumSimple (SimplePitch C Nat)   = 0
      fromEnumSimple (SimplePitch C Sharp) = 1
      fromEnumSimple (SimplePitch D Nat)   = 2
      fromEnumSimple (SimplePitch D Sharp) = 3
      fromEnumSimple (SimplePitch E Nat)   = 4
      fromEnumSimple (SimplePitch F Nat)   = 5
      fromEnumSimple (SimplePitch F Sharp) = 6
      fromEnumSimple (SimplePitch G Nat)   = 7
      fromEnumSimple (SimplePitch G Sharp) = 8
      fromEnumSimple (SimplePitch A Nat)   = 9
      fromEnumSimple (SimplePitch A Sharp) = 10
      fromEnumSimple (SimplePitch B Nat)   = 11
  
  toEnum 0   = SimplePitch C Nat
  toEnum 1   = SimplePitch C Sharp
  toEnum 2   = SimplePitch D Nat
  toEnum 3   = SimplePitch D Sharp
  toEnum 4   = SimplePitch E Nat
  toEnum 5   = SimplePitch F Nat
  toEnum 6   = SimplePitch F Sharp
  toEnum 7   = SimplePitch G Nat
  toEnum 8   = SimplePitch G Sharp
  toEnum 9   = SimplePitch A Nat
  toEnum 10  = SimplePitch A Sharp
  toEnum 11  = SimplePitch B Nat

  toEnum i  = toEnum $ i `mod` 12
  
  