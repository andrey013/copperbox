
module Sound.Bala.Base.PitchRep where

data PitchLetter = A | B | C | D | E | F | G 
  deriving (Eq,Enum,Ord,Show)

data Accidental = Nat | Sharp | SharpSharp | Flat | FlatFlat  
                | Sharpi Int | Flati Int
  deriving Eq
  
  
data Pitch = Pitch
  { pitch       :: PitchLetter
  , accidental  :: Accidental
  , octave      :: Int 
  , cents       :: Int 
  }
  deriving Eq
  
  
instance Show Accidental where
  showsPrec _ Nat         = showString ""
  showsPrec _ Sharp       = showChar '#'
  showsPrec _ SharpSharp  = showString "x"
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
  
    
  