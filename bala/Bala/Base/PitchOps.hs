{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

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

-- | Semitones is the basis for Pitch arithmetic
class Semitones a where semitones :: a -> Int

-- | Add and subtract semitones
class SemitoneExtension a where
  -- | Add a semitone to a /pitched value/.
  addSemi :: a -> Int -> a
  -- | Subtract a semitone from a /pitched value/.
  subSemi :: a -> Int -> a

class OctaveExtension a where
  -- | Add an octave to a /pitched value/.  
  addOve  :: a -> Int -> a
  -- | Subtract an octave from a /pitched value/.
  subOve  :: a -> Int -> a


-- | Sharpen or flatten a /pitched value/.
class AlterPitch a where  
  sharp   :: a -> a
  flat    :: a -> a

-- | Are two /pitched values/ enharmonically equal? 
class Enharmonic a where
  enharmonic :: a -> a -> Bool

class SemitoneDisplacement a where
  -- | The semitone distance from one element to another, and the direction 
  -- of the distance. 'Upwards' if the second element had a higher pitch than 
  -- the first, otherwise 'Downwards'
  semitoneDisplacement :: a -> a -> (Direction,Int)
  
  -- | As per 'semitoneDisplacement' but just return the distance.
  semitoneDistance     :: a -> a -> Int
  
  -- | As per 'semitoneDisplacement' but just return the direction.   
  semitoneDirection    :: a -> a -> Direction
  
  semitoneDistance  a a' = snd $ semitoneDisplacement a a'
  semitoneDirection a a' = fst $ semitoneDisplacement a a'

class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a


  
--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

-- The semitone displacement upwards from C
instance Semitones PitchLetter where
  semitones l = fromEnum (PitchLabel l Nat)


-- How many semitones is the pitch changed by its accidental? 
instance Semitones Accidental where
  semitones Nat       = 0
  semitones (Sharp i) = i
  semitones (Flat i)  = negate i

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a

instance Semitones Pitch where
  semitones (Pitch l o s c) = 
    let (cc,_) = explode100 c in  12 * o + s + cc
     
    
-- works for ordered elts not rotated ones
instance SemitoneDisplacement Pitch where
  semitoneDisplacement p p' = 
    let d = semitones p - semitones p'
    in case signum d of
      (-1) -> (Downwards, abs d)
      _    -> (Upwards, d)

-- | Pitch labels must handle rotation
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
  
instance EncodePitch Int where
  toPitch i = pitchValue i
  fromPitch p = centValue p 


instance Extract Pitch PitchLabel where
  extract (Pitch l _ _ _) = l

  
  
-- | A /smart constructor/. It doesn't need semitones stating as it 
-- derives semitones from the 'PitchLabel'.
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

-- | Spell the 'PitchLabel' according to the 'PitchLetter', changing the 
-- accidental as required, for instance:
--
-- >   spell (PitchLabel F (Sharp 1)) G = (PitchLabel G (Flat 1))
spell :: PitchLabel -> PitchLetter -> PitchLabel
spell lbl l' = 
  let (dr,d) = semitoneDisplacement lbl (PitchLabel l' Nat)
      dist   = case dr of Upwards -> negate d; _ -> d
      a'     = alter Nat dist
  in PitchLabel l' a'
  
  
  
alter :: Accidental -> Int -> Accidental
alter a i  = toEnum $ fromEnum a + i






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
  


--------------------------------------------------------------------------------
-- Num instances
--------------------------------------------------------------------------------

instance Num Accidental where
  a + b = toEnum $ fromEnum a + fromEnum b
                           
  a - b = toEnum $ fromEnum a - fromEnum b
    
  a * b = toEnum $ fromEnum a * fromEnum a

  abs (Flat i) = Sharp i  
  abs a        = a
  
  
  signum Nat       = Nat
  signum (Sharp _) = Sharp 1
  signum _         = Flat 1

    
  fromInteger = toEnum . fromIntegral   
  
instance Num Pitch where
  (Pitch l o s c) + (Pitch _ o' s' c') = Pitch l' (o + o' + co) s'' c''
    where (cs,c'') = explode100 (c + c')
          (co,s'') = explode12 (s + s' + cs)
          l' = toEnum $ s'' + fromEnum l


  (Pitch l o s c) - (Pitch _ o' s' c') = 
    let (cs,c'') = explode100 (c - c')
        (co,s'') = explode12 (s - s' + cs)
        l' = toEnum $ s'' - fromEnum l
    in Pitch l' (o - o' + co) s'' c''
  
  p * p' = 
    let semil     = fromIntegral $ semitones p
        semir     = fromIntegral $ semitones p'
        centl     = (fromIntegral $ cents p) / 100.0
        centr     = (fromIntegral $ cents p') / 100.0
        sp        = (semil + centl) * (semir + centr)
        (semis,c) = properFraction $ (semil + centl) * (semir + centr)
        (o,s)     = explode12 semis
    in Pitch (toEnum semis) o s (round (100 * c)) 
  
  abs p     = let (Pitch l o s _) = fromInteger $ fromIntegral $ abs $ semitones p
              in Pitch l o s (cents p)
              
  signum p  = case semitones p `compare` 0 of
                EQ -> Pitch (toEnum 0) 0 0 0
                GT -> Pitch (toEnum 0) 0 1 0
                LT -> Pitch (toEnum (-1)) (-1) 11 0
                
  
  -- note, this is different to midi - middle C here is 48 (in midi it is 60)
  fromInteger i = let i' = fromIntegral i; (o,s) = explode12  i'; l = toEnum i'
                  in Pitch l o s 0   



  
--------------------------------------------------------------------------------
-- Deco instances
--------------------------------------------------------------------------------


instance Deco Pitch where 
  deco = decoPitch

-- | Parsec parser for 'Pitch'.
decoPitch :: Parser Pitch
decoPitch = buildPitch <$> deco
                       <*> option 4 positiveInt 
                       <*> option 0 signedInt
                       
                       
instance Deco PitchLabel where
  deco = decoPitchLabel

-- | Parsec parser for 'PitchLabel'.
decoPitchLabel :: Parser PitchLabel
decoPitchLabel = PitchLabel <$> decoPitchLetter <*> decoAccidental


-- hyper abstract - but is it simpler, clearer? 
-- 
-- decoPitchLabel = PitchLabel <$> deco <*> deco
-- or, 
-- decoPitchLabel' = deco2 PitchLabel
-- deco2 :: (Deco a, Deco b) => (a -> b -> c) -> Parser c
-- deco2 fn = fn <$> deco <*> deco 


instance Deco PitchLetter where
  deco = decoPitchLetter

-- | Parsec parser for 'PitchLetter'.
decoPitchLetter :: Parser PitchLetter    
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

-- | Parsec parser for 'Accidental'.
--
-- A flat is one or more /b/, a sharp is either all /\#/ or 
-- /\#/ [1],  /x/ [2], /x\#/ [3], /xx/ [4], /xx\#/ [5], /xxx/ [6], etc... 
decoAccidental :: Parser Accidental 
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
        
-- | Print sharps as with muliple '#'s only
instance Affi Accidental where
  affi Nat        = showString ""
  affi (Sharp i)  = showString (replicate i '#')                          
  affi (Flat i)   = showString (replicate i 'b')     
  
  
instance Affi PitchLetter where
    affi = shows
    
        