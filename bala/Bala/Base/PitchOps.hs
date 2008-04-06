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
-- Pitch operations
--
--------------------------------------------------------------------------------

module Bala.Base.PitchOps where

import Bala.Base.PitchRep

import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec


--------------------------------------------------------------------------------
-- typeclasses
--------------------------------------------------------------------------------

class SemitoneExtension a where 
  addSemi :: a -> Int -> a
  subSemi :: a -> Int -> a

class OctaveExtension a where   
  addOve  :: a -> Int -> a
  subOve  :: a -> Int -> a
    
class AlterPitch a where  
  sharp   :: a -> a
  flat    :: a -> a
 
class Enharmonic a where
  enharmonic :: a -> a -> Bool

class SemitoneDisplacement a where 
  semitoneDisplacement :: a -> a -> (Direction,Int)
  semitoneDistance     :: a -> a -> Int   
  semitoneDirection    :: a -> a -> Direction
  
  semitoneDistance  a a' = snd $ semitoneDisplacement a a'
  semitoneDirection a a' = fst $ semitoneDisplacement a a'

class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a

instance EncodePitch Int where
  toPitch i = pitchValue i
  fromPitch p = centValue p 
  
--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------
 
    
-- works for ordered elts not rotated ones
instance SemitoneDisplacement Pitch where
  semitoneDisplacement p p' = 
    let d = semitones p - semitones p'
    in case signum d of
      (-1) -> (Downwards, abs d)
      _    -> (Upwards, d)

-- must handle rotation
instance SemitoneDisplacement PitchLabel where
  semitoneDisplacement l l' = 
    let d = countTo succ l l'
    in if (d > 6) then (Downwards, 12 - d) else (Upwards, abs d)
                          





instance AlterPitch Accidental where  
  sharp a = succ a
  flat a  = pred a
  
instance AlterPitch PitchLabel where  
  sharp (PitchLabel l a) = PitchLabel l (sharp a)
  flat (PitchLabel l a)  = PitchLabel l (flat a)
  
instance AlterPitch Pitch where  
  sharp (Pitch l o s c) = let (oc,s') = explode12 $ s + 1
                          in Pitch (sharp l) (o+oc) s' c
  flat (Pitch l o s c)  = let (oc,s') = explode12 $ s - 1
                          in Pitch (flat l) (o+oc) s' c
  
    

-- (31/3/08) this is most likely wrong, we should keep the letter 
-- and extend the alteration
instance SemitoneExtension PitchLabel where
  addSemi pl i = toEnum $ (fromEnum pl) + i
  subSemi pl i = toEnum $ (fromEnum pl) - i
  
{-
  addSemi (PitchLabel l a) i = PitchLabel l (a `successor` i)
  subSemi (PitchLabel l a) i = PitchLabel l (a `predecessor` i)
-}

instance SemitoneExtension Pitch where
  -- oc is "octave-carry" this isn't a very descriptive implementation
  -- and at some point should be done better
  (Pitch l o s c) `addSemi` i = 
    let (oc,s') = explode12 $ s + i
    in Pitch (l `addSemi` i) (o + oc) s' c
  
  
  (Pitch l o s c) `subSemi` i = 
    let (oc,s') = explode12 $ s - i
    in Pitch (l `subSemi` i) (o - oc) s' c
  
  
    
instance OctaveExtension Pitch where   
  (Pitch l o s c) `addOve` i = Pitch l (o + i) s c
  (Pitch l o s c) `subOve` i = Pitch l (o - i) s c

instance Enharmonic PitchLabel where
  l `enharmonic` l' = semitones l == semitones l'
  

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
    

-- destructor
unwrapPitchLabel (Pitch l _ _ _) = l

    
-- pitch ops -- adding intervals etc need a naming scheme


alter :: Accidental -> Int -> Accidental
alter a i  = toEnum $ fromEnum a + i

spell :: PitchLabel -> PitchLetter -> PitchLabel
spell lbl l' = 
  let (dr,d) = semitoneDisplacement lbl (PitchLabel l' Nat)
      dist   = case dr of Upwards -> negate d; _ -> d
      a'     = alter Nat dist
  in PitchLabel l' a'




spellWithSharps :: PitchLabel -> PitchLabel
spellWithSharps (PitchLabel l a)   = 
  toEnum $ semitones l + semitones a
    
    
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


   
octaveDisplacement oct            = (oct - 4) * 12  
  



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
-- Deco instances
--------------------------------------------------------------------------------


instance Deco Pitch where 
  deco = decoPitch

decoPitch :: Parser Pitch
decoPitch = buildPitch <$> deco
                       <*> option 4 positiveInt 
                       <*> option 0 signedInt
                       
                       
instance Deco PitchLabel where
  deco = decoPitchLabel

decoPitchLabel = PitchLabel <$> decoPitchLetter <*> decoAccidental
-- or decoPitchLabel = PitchLabel <$> deco <*> deco

decoPitchLabel' = deco2 PitchLabel
 
deco2 :: (Deco a, Deco b) => (a -> b -> c) -> Parser c
deco2 fn = fn <$> deco <*> deco 


instance Deco PitchLetter where
  deco = decoPitchLetter
    
decoPitchLetter = letter <$> oneOf "ABCDEFG" 
  where 
    letter 'A' = A
    letter 'B' = B
    letter 'C' = C
    letter 'D' = D
    letter 'E' = E
    letter 'F' = F
    letter 'G' = G 
    
instance Deco Accidental where 
  deco = decoAccidental

-- accidental either all '#' or 
-- "#" (1), "x" (2), "x#" (3), "xx" (4), "xx#" (5), "xxx" (6), etc...
decoAccidental = choice [sharp, flat, nat]
  where 
    sharp         = plainsharp <|> doublesharp
    plainsharp    = Sharp <$> countChar1 '#'
    doublesharp   = mkDbl <$> countChar1 'x' <*> option 0 (countChar1 '#')
    flat          = Flat  <$> countChar1 'b'
    nat           = return Nat
    mkDbl dbl sgl = Sharp $ dbl * 2 + sgl
    countChar1    = counting1 . char 




--------------------------------------------------------------------------------
-- Affi instances
--------------------------------------------------------------------------------
instance Affi Pitch where
  affi (Pitch l o s c) | c == 0    = affi l . shows o 
                       | c > 0     = affi l . shows o . showChar '+' . shows c
                       | otherwise = affi l . shows o . shows c
    
    
instance Affi PitchLabel where 
    affi (PitchLabel l a) = affi l . affi a
        
-- print sharps as with muliple '#'s only
instance Affi Accidental where
  affi Nat        = showString ""
  affi (Sharp i)  = showString (replicate i '#')                          
  affi (Flat i)   = showString (replicate i 'b')     
  
  
instance Affi PitchLetter where
    affi = shows
    
        