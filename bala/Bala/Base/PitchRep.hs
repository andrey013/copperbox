
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

data Pitch = Pitch { 
    pitch       :: PitchLetter,
    accidental  :: Accidental,
    octave      :: Int,
    cents       :: Int 
  }
  deriving Eq
    
data PitchLetter = A | B | C | D | E | F | G 
  deriving (Eq,Enum,Ord,Show)

data Accidental = Nat | Sharp | SharpSharp | Flat | FlatFlat  
                | Sharpi Int | Flati Int
  deriving Eq

instance Ord Pitch where
  p1 `compare` p2 = (toCents p1) `compare` (toCents p2)

newtype Cents = Cents {unCents :: Int}
  deriving (Eq,Ord,Show)

toCents (Pitch l a o c) = Cents $ 
  (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c


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



  
--------------------------------------------------------------------------------
-- operations... (new file?)
--------------------------------------------------------------------------------

-- pitch ops -- adding intervals etc need a naming scheme

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
  