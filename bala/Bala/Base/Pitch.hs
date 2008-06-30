
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
  
  -- * Operations
  spell,
  
  addOve, subOve, semitoneDistance, semitoneDirection,
  
  unaltered,
  
  -- * Named elements
  -- $nameddoc 
  middle_c,
  c4, d4, e4, f4, g4, a4, b4,
  c4is, d4es, d4is, e4es, f4is, g4es, g4is, a4es, a4is, b4es,
  
  c3, d3, e3, f3, g3, a3, b3,
  c3is, d3es, d3is, e3es, f3is, g3es, g3is, a3es, a3is, b3es,
  
  c5, d5, e5, f5, g5, a5, b5,
  c5is, d5es, d5is, e5es, f5is, g5es, g5is, a5es, a5is, b5es,
  
  c6, d6, e6, f6, g6, a6, b6,
  c6is, d6es, d6is, e6es, f6is, g6es, g6is, a6es, a6is, b6es
  
  
  ) where

import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec


--------------------------------------------------------------------------------
-- Datatypes

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

data Accidental = DoubleFlat | Flat | Nat | Sharp | DoubleSharp 
  deriving (Eq,Enum,Read,Show)



--------------------------------------------------------------------------------
-- | Constructors and selectors


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

naturalNote     :: PitchLetter -> PitchName
naturalNote z   = PitchName z Nat

sharpNote       :: PitchLetter -> PitchName
sharpNote z     = PitchName z Sharp

flatNote        :: PitchLetter -> PitchName
flatNote z      = PitchName z Flat


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
--
-- If the pitch distance is greater than two semitones, return the original 
-- spelling 
spell :: PitchName -> PitchLetter -> PitchName
spell lbl l' = if (abs dist > 2) then lbl else PitchName l' (alter dist)
  where
    (dr,d) = semitoneDisplacement lbl (PitchName l' Nat)
    dist   = case dr of Upwards -> negate d; _ -> d 

    alter :: Int -> Accidental
    alter 0     = Nat
    alter (-1)  = Flat
    alter 1     = Sharp
    alter  (-2) = DoubleFlat
    alter 2     = DoubleSharp
    alter z     = error $ "alter " ++ show z


spellWithSharps :: PitchName -> PitchName
spellWithSharps lbl   = 
  toEnum $ semitoneCount lbl


unaltered :: PitchName -> Bool  
unaltered p = pch_name_accidental p == Nat

octaveDisplacement oct            = (oct - 4) * 12  


centValue :: Pitch -> Int
centValue (Pitch l o s c) 
  = octaveToCents o  + semitonesToCents s + c
  
semitonesToCents :: Int -> Int
semitonesToCents = (100 *)

octaveToCents :: Int -> Int
octaveToCents   = (12 * 100 *)

  

-- | Add an octave to a pitched value.  
addOve  :: Pitched a => a -> Int -> a
addOve e = addSemi e . (12 *)

-- | Subtract an octave from a pitched value.
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
    
--------------------------------------------------------------------------------
-- Named elements
-- $nameddoc 
-- Pre-defined pitches. Middle c is @c4@, octaves start on c. 
-- Sharp and flat notes follow the LilyPond convention with suffix of @is@ for 
-- a sharp and @es@ for a flat.

pchNat n o    = pitch (naturalNote n) o
pchSharp n o  = pitch (sharpNote n) o
pchFlat n o   = pitch (flatNote n) o

middle_c :: Pitch
middle_c = pchNat C 4

c4, d4, e4, f4, g4, a4, b4, 
    c4is, d4es, d4is, e4es, f4is, g4es, g4is, a4es, a4is, b4es :: Pitch
c4    = pchNat C 4
d4    = pchNat D 4
e4    = pchNat E 4
f4    = pchNat F 4
g4    = pchNat G 4
a4    = pchNat A 4
b4    = pchNat B 4
c4is  = pchSharp C 4
d4es  = pchFlat D 4
d4is  = pchSharp D 4
e4es  = pchFlat E 4
f4is  = pchSharp F 4
g4es  = pchFlat G 4
g4is  = pchSharp G 4
a4es  = pchFlat A 4
a4is  = pchSharp A 4
b4es  = pchFlat B 4


c3, d3, e3, f3, g3, a3, b3, 
    c3is, d3es, d3is, e3es, f3is, g3es, g3is, a3es, a3is, b3es :: Pitch
c3    = pchNat C 3
d3    = pchNat D 3
e3    = pchNat E 3
f3    = pchNat F 3
g3    = pchNat G 3
a3    = pchNat A 3
b3    = pchNat B 3
c3is  = pchSharp C 3
d3es  = pchFlat D 3
d3is  = pchSharp D 3
e3es  = pchFlat E 3
f3is  = pchSharp F 3
g3es  = pchFlat G 3
g3is  = pchSharp G 3
a3es  = pchFlat A 3
a3is  = pchSharp A 3
b3es  = pchFlat B 3

c5, d5, e5, f5, g5, a5, b5, 
    c5is, d5es, d5is, e5es, f5is, g5es, g5is, a5es, a5is, b5es :: Pitch
c5    = pchNat C 5
d5    = pchNat D 5
e5    = pchNat E 5
f5    = pchNat F 5
g5    = pchNat G 5
a5    = pchNat A 5
b5    = pchNat B 5
c5is  = pchSharp C 5
d5es  = pchFlat D 5
d5is  = pchSharp D 5
e5es  = pchFlat E 5
f5is  = pchSharp F 5
g5es  = pchFlat G 5
g5is  = pchSharp G 5
a5es  = pchFlat A 5
a5is  = pchSharp A 5
b5es  = pchFlat B 5

c6, d6, e6, f6, g6, a6, b6, 
    c6is, d6es, d6is, e6es, f6is, g6es, g6is, a6es, a6is, b6es :: Pitch
c6    = pchNat C 6
d6    = pchNat D 6
e6    = pchNat E 6
f6    = pchNat F 6
g6    = pchNat G 6
a6    = pchNat A 6
b6    = pchNat B 6
c6is  = pchSharp C 6
d6es  = pchFlat D 6
d6is  = pchSharp D 6
e6es  = pchFlat E 6
f6is  = pchSharp F 6
g6es  = pchFlat G 6
g6is  = pchSharp G 6
a6es  = pchFlat A 6
a6is  = pchSharp A 6
b6es  = pchFlat B 6



