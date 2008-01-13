
module Sound.Bala.Base.PitchRep where

import Sound.Bala.Base.ReadPExtra

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)
import Data.Char
import Text.ParserCombinators.ReadP

data Pitch = Pitch
  { pitch       :: PitchLetter
  , accidental  :: Accidental
  , octave      :: Int 
  , cents       :: Int 
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
  (octaveDisplacement o * 100) + ((root l + alteration a) * 100) + c

root C = 0
root D = 2
root E = 4
root F = 5
root G = 7
root A = 9
root B = 11
 
alteration Nat        = 0
alteration Sharp      = 1
alteration SharpSharp = 2
alteration Flat       = (-1)
alteration FlatFlat   = (-2)
alteration (Sharpi i) = i
alteration (Flati i)  = 0 - i

octaveDisplacement oct            = (oct - 4) * 12  
  
mod12 i = i `mod` 12
mod7  i = i `mod` 7  

  
class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a

instance Read Pitch where 
  readsPrec i s = readP_to_S readPitch s

readPitch :: ReadP Pitch
readPitch = Pitch <$> readPitchLetter 
                  <*> readAccidental
                  <*> option 4 positiveInt 
                  <*> option 0 signedInt
  where positiveInt = read <$> many1 (satisfy isDigit) 
        signedInt   = (\ a b -> read (a:b)) <$> sign <*> many1 (satisfy isDigit)
        sign        = (char '+') +++ (char '-')                
                
instance Read PitchLetter where 
  readsPrec i s = readP_to_S readPitchLetter s
  
readPitchLetter = letter <$> satisfy (\c -> c >= 'A' && c <= 'G') 
  where 
    letter 'A' = A
    letter 'B' = B
    letter 'C' = C
    letter 'D' = D
    letter 'E' = E
    letter 'F' = F
    letter 'G' = G


instance Read Accidental where 
  readsPrec i s = readP_to_S readAccidental s
  
readAccidental = accident <$> munch1 ((==) '#') +++ munch ((==) 'b')
  where accident ""       = Nat
        accident "#"      = Sharp
        accident "##"     = SharpSharp
        accident "b"      = Flat
        accident "bb"     = FlatFlat
        accident ('#':xs) = Sharpi (1+ length xs)
        accident ('b':xs) = Flati (1+ length xs)

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


instance Show Pitch where
  showsPrec _ (Pitch p a o i) | i == 0    = root
                              | i < 0     = root . shows i
                              | otherwise = root . showChar '+' . shows i
    where root = shows p . shows a . shows o
  


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
  