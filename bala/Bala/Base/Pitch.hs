
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
  -- * Datatypes (Pitch is opaque) 
  Pitch, PitchName(..), PitchLetter(..), Accidental(..),
  
  -- * Construct and deconstruct pitches
  pitch, withCents, 
  unPitch, pitchName, pitchLetter, pitchAccidental, 
  octaveMeasure, semitoneMeasure, centMeasure,
  pitchMeasures,
  
  -- * Typeclasses
  Semitones(..),  Pitched(..), EncodePitch(..),
  
  spell,
  
  addOve, subOve, semitoneDistance, semitoneDirection,
  
  unaltered

  ) where

import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Note - there is redundancy between pitch_label and semitones, operations
-- on Pitch must take care to account for both.

 
data Pitch = Pitch {
    pch_label       :: PitchName,
    pch_octave      :: Int,
    pch_semitones   :: Int,
    pch_cents       :: Int 
  }
  deriving (Eq,Read,Show)

-- | Represent pitches independent of octave   
data PitchName = PitchName {
    pch_name_pitch_letter :: PitchLetter,
    pch_name_accidental   :: Accidental
  }
  deriving (Eq,Read,Show) 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Ord,Read,Show)

data Accidental = Nat | Sharp | Flat | DoubleSharp | DoubleFlat
  deriving (Eq,Enum,Read,Show)



-- * Constructors and selectors


-- | A /smart constructor/. It doesn't need semitones stating as it 
-- derives semitones from the 'PitchName'.
pitch :: PitchName -> Int -> Pitch
pitch lbl o = Pitch lbl o (semitoneCount lbl) 0
  where
    semis C = 0
    semis D = 2
    semis E = 4
    semis F = 5
    semis G = 7
    semis A = 9
    semis B = 11

withCents :: Pitch -> Int -> Pitch
withCents p i = let c = pch_cents p in p {pch_cents=c+i}

unPitch :: Pitch -> (PitchName,Int,Int,Int)
unPitch (Pitch lbl o s c) = (lbl,o,s,c)

pitchName :: Pitch -> PitchName
pitchName (Pitch lbl _ _ _) = lbl

pitchLetter :: Pitch -> PitchLetter
pitchLetter (Pitch (PitchName l _) _ _ _) = l

pitchAccidental :: Pitch -> Accidental
pitchAccidental (Pitch (PitchName _ a) _ _ _) = a


octaveMeasure :: Pitch -> Int
octaveMeasure (Pitch _ o _ _) = o

semitoneMeasure :: Pitch -> Int
semitoneMeasure (Pitch _ _ s _) = s

centMeasure :: Pitch -> Int
centMeasure (Pitch _ _ _ c) = c

pitchMeasures :: Pitch -> (Int,Int,Int)
pitchMeasures (Pitch _ o s c) = (o,s,c)


--------------------------------------------------------------------------------
-- * Typeclasses for pitched values

-- | Semitones are the basis for Pitch arithmetic
class Semitones a where semitoneCount   :: a -> Int

-- | Pitch operations, minimal complete definition:
-- addSemi, subSemi, semitoneDisplacement.
class (Semitones a) => Pitched a where
    
    -- | Add a semitone to a pitched value.
    addSemi               :: a -> Int -> a
    
    -- | Subtract a semitone from a pitched value.
    subSemi               :: a -> Int -> a
  
    -- | Sharpen a pitched value.
    sharpen               :: a -> a
    
    -- | Flatten a pitched value.
    flatten               :: a -> a
    
    -- | Are two pitched values enharmonically equal? 
    enharmonic            :: a -> a -> Bool
    
    -- | The semitone distance from one element to another, and the direction 
    -- of the distance. 'Upwards' if the second element had a higher pitch than 
    -- the first, otherwise 'Downwards'
    semitoneDisplacement  :: a -> a -> (Direction,Int)
    

  
 
    
    sharpen             = (flip addSemi) 1
    flatten             = (flip subSemi) 1
    enharmonic a b      = semitoneCount a == semitoneCount b     



-- | Convert to and from the primary pitch representation.
class EncodePitch a where 
    -- | Convert to a Pitch.
    toPitch       :: a -> Pitch 
    -- | Convert from a Pitch. 
    fromPitch     :: Pitch -> a
  
    
        
--------------------------------------------------------------------------------
-- 

-- pitch ops -- adding intervals etc need a naming scheme

-- | Spell the 'PitchName' according to the 'PitchLetter', changing the 
-- accidental as required, for instance:
--
-- >   spell (PitchName F (Sharp 1)) G = (PitchName G (Flat 1))
spell :: PitchName -> PitchLetter -> PitchName
spell lbl l' = 
  let (dr,d) = semitoneDisplacement lbl (PitchName l' Nat)
      dist   = case dr of Upwards -> negate d; _ -> d
      a'     = alter Nat dist
  in PitchName l' a'
  
  
  
alter :: Accidental -> Int -> Accidental
alter a i  = toEnum $ fromEnum a + i


spellWithSharps :: PitchName -> PitchName
spellWithSharps lbl   = 
  toEnum $ semitoneCount lbl


unaltered :: PitchName -> Bool  
unaltered p = pch_name_accidental p == Nat

octaveDisplacement oct            = (oct - 4) * 12  


centValue :: Pitch -> Int
centValue (Pitch l o s c) 
  = (octaveDisplacement o * 100) + (s * 100) + c
  
semitonesToCents :: Int -> Int
semitonesToCents = (1000 *)

octaveToCents :: Int -> Int
octaveToCents   = (12 * 1000 *)

  

-- | Add an octave to a /pitched value/.  
addOve  :: Pitched a => a -> Int -> a
addOve e = addSemi e . (12 *)

-- | Subtract an octave from a /pitched value/.
subOve  :: Pitched a => a -> Int -> a
subOve e = subSemi e . (12 *)


-- | As per 'semitoneDisplacement' but just return the distance.
semitoneDistance    :: Pitched a => a -> a -> Int
semitoneDistance    = snd `dyap` semitoneDisplacement

-- | As per 'semitoneDisplacement' but just return the direction.   
semitoneDirection   :: Pitched a => a -> a -> Direction
semitoneDirection   = fst `dyap` semitoneDisplacement  

--------------------------------------------------------------------------------
-- Instances

instance Semitones Pitch where
  semitoneCount (Pitch l o s c) = 
    let (cc,_) = explode100 c in  (12 * o) + s + cc
    
    
instance Pitched Pitch where
  addSemi (Pitch l o s c) i = 
    let (oc,s') = explode12 $ s + i in Pitch (l `addSemi` i) (o + oc) s' c
    
  subSemi (Pitch l o s c) i = 
    let (oc,s') = explode12 $ s - i in Pitch (l `subSemi` i) (o - oc) s' c

  semitoneDisplacement p p' = 
    let d = semitoneCount p' - semitoneCount p
    in case signum d of
      (-1) -> (Downwards, abs d)
      _    -> (Upwards, d)
      
      
      
-- C-nat = 0 
instance Semitones PitchLetter where
  semitoneCount l = fromEnum (PitchName l Nat)

instance Semitones Accidental where
  semitoneCount Nat          = 0
  semitoneCount Sharp        = 1
  semitoneCount DoubleSharp  = 2
  semitoneCount Flat         = (-1)
  semitoneCount DoubleFlat   = (-2)

instance Semitones PitchName where
  semitoneCount (PitchName l a) = semitoneCount l + semitoneCount a
  


-- | The names generated by toEnum favour sharps, pitches may need re-spelling.     
instance Pitched PitchName where
  addSemi l i = toEnum $ (fromEnum l) + i
  subSemi l i = toEnum $ (fromEnum l) - i
  
  semitoneDisplacement l l' = 
    let d = countTo succ l l'
    in if (d > 6) then (Downwards, 12 - d) else (Upwards, abs d)


      
--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum PitchLetter where 
  fromEnum C = 0
  fromEnum D = 1
  fromEnum E = 2
  fromEnum F = 3
  fromEnum G = 4
  fromEnum A = 5
  fromEnum B = 6
  
  toEnum 0   = C
  toEnum 1   = D
  toEnum 2   = E
  toEnum 3   = F
  toEnum 4   = G
  toEnum 5   = A
  toEnum 6   = B

  toEnum i  = toEnum $ mod7 i

  
instance Enum PitchName where 
  fromEnum (PitchName l a) = mod12 $ fn l + semitoneCount a
    where fn C = 0
          fn D = 2
          fn E = 4
          fn F = 5
          fn G = 7
          fn A = 9
          fn B = 11

  
  toEnum 0   = PitchName C Nat
  toEnum 1   = PitchName C Sharp
  toEnum 2   = PitchName D Nat
  toEnum 3   = PitchName D Sharp
  toEnum 4   = PitchName E Nat
  toEnum 5   = PitchName F Nat
  toEnum 6   = PitchName F Sharp
  toEnum 7   = PitchName G Nat
  toEnum 8   = PitchName G Sharp
  toEnum 9   = PitchName A Nat
  toEnum 10  = PitchName A Sharp
  toEnum 11  = PitchName B Nat

  toEnum i  = toEnum $ mod12 i
  




--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------


instance Ord Accidental where
  compare a a' = fromEnum a `compare` fromEnum a' 

instance Ord PitchName where
  compare a a' = fromEnum a `compare` fromEnum a' 
  
instance Ord Pitch where
  compare (Pitch _ o s c) (Pitch _ o' s' c') = a `compare` b
    where 
      (sc,cc)   = explode100 c
      (sc',cc') = explode100 c'
      a         = octaveToCents o + semitonesToCents (s + sc) + cc
      b         = octaveToCents o' + semitonesToCents (s' + sc') + cc'
    



--------------------------------------------------------------------------------
-- Num instances
--------------------------------------------------------------------------------

{-
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
-}

  
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
    let semil     = fromIntegral $ semitoneCount p
        semir     = fromIntegral $ semitoneCount p'
        centl     = (fromIntegral $ pch_cents p) / 100.0
        centr     = (fromIntegral $ pch_cents p') / 100.0
        sp        = (semil + centl) * (semir + centr)
        (semis,c) = properFraction $ (semil + centl) * (semir + centr)
        (o,s)     = explode12 semis
    in Pitch (toEnum semis) o s (round (100 * c)) 
  
  abs p     = let (Pitch l o s _) = fromInteger $ fromIntegral $ abs $ semitoneCount p
              in Pitch l o s (pch_cents p)
              
  signum p  = case semitoneCount p `compare` 0 of
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
decoPitch = fn <$> deco <*> option 4 positiveInt <*> option 0 signedInt
  where
    fn pl o c = pitch pl o `withCents` c                       
                       
                       
instance Deco PitchName where
  deco = decoPitchName

-- | Parsec parser for 'PitchName'.
decoPitchName :: Parser PitchName
decoPitchName = PitchName <$> decoPitchLetter <*> decoAccidental


-- hyper abstract - but is it simpler, clearer? 
-- 
-- decoPitchName = PitchName <$> deco <*> deco
-- or, 
-- decoPitchName' = deco2 PitchName
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
decoAccidental :: Parser Accidental 
decoAccidental = withLongestString (lexString) $ 
  [ ("##", DoubleSharp)
  , ("#", Sharp)
  , ("bb", DoubleFlat)
  , ("b", Flat)
  , ("", Nat)
  ]

--------------------------------------------------------------------------------
-- Affi instances
--------------------------------------------------------------------------------

instance Affi Pitch where
  affi (Pitch l o s c) | c == 0    = affi l . shows o 
                       | c > 0     = affi l . shows o . showChar '+' . shows c
                       | otherwise = affi l . shows o . shows c
    
    
instance Affi PitchName where 
    affi (PitchName l a) = affi l . affi a
        
-- | Print sharps as with muliple '#'s only
instance Affi Accidental where
  affi Nat          = id
  affi Sharp        = showChar '#'
  affi DoubleSharp  = showString "##" 
  affi Flat         = showChar 'b'     
  affi DoubleFlat   = showString "bb" 
  
instance Affi PitchLetter where
    affi = shows
    



