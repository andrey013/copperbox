
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
  
  -- * Construct and deconstruct pitches
  pitch, 
  unPitch, pitchName, pitchLetter, pitchAccidental, 
  octaveMeasure, semitoneMeasure,
  
  -- * Typeclasses
  Pitched(..), EncodePitch(..),
  
  -- * Operations
  spell,
  
  addOve, subOve, semitoneDistance, semitoneDirection,
  
  unaltered,
  
  -- * Named elements
  -- $nameddoc 
  c_natural, d_natural, e_natural, f_natural, g_natural, a_natural, b_natural,
  c_sharp, d_flat, d_sharp, e_flat, f_sharp, 
  g_flat, g_sharp, a_flat, a_sharp, b_flat,


  
  ) where

import Bala.Base.BaseExtra

-- Use the Pitch format defined in HNotate
import HNotate.Pitch

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec


--------------------------------------------------------------------------------
-- Datatypes

-- imported from HNotate

--------------------------------------------------------------------------------
-- | Constructors and selectors


-- | A /smart constructor/. It doesn't need semitones stating as it 
-- derives semitones from the 'PitchName'.
-- Update - no longer smart now that we don't store semitone_count
pitch :: PitchLabel -> Int -> Pitch
pitch lbl o = Pitch (pch_lbl_letter lbl) (pch_lbl_accidental lbl) o 



unPitch :: Pitch -> (PitchLetter,Accidental,Int)
unPitch (Pitch l a o) = (l,a,o)

pitchName :: Pitch -> PitchLabel
pitchName (Pitch l a _) = PitchLabel l a

pitchLetter :: Pitch -> PitchLetter
pitchLetter = pch_letter

pitchAccidental :: Pitch -> Accidental
pitchAccidental = pch_accidental

octaveMeasure :: Pitch -> Int
octaveMeasure = pch_octave

semitoneMeasure :: Pitch -> Int
semitoneMeasure = semitones


naturalNote     :: PitchLetter -> PitchLabel
naturalNote z   = PitchLabel z Nat

sharpNote       :: PitchLetter -> PitchLabel
sharpNote z     = PitchLabel z Sharp

flatNote        :: PitchLetter -> PitchLabel
flatNote z      = PitchLabel z Flat


--------------------------------------------------------------------------------
-- * Typeclasses for pitched values

{-
-- defined in HNotate.Pitch

-- | Semitones are the basis for Pitch arithmetic
class Semitones a where semitones   :: a -> Int

-}
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
    enharmonic a b      = semitones a == semitones b     



-- | Convert to and from the primary pitch representation.
class EncodePitch a where 
    -- | Convert to a Pitch.
    toPitch       :: a -> Pitch 
    -- | Convert from a Pitch. 
    fromPitch     :: Pitch -> a
  
    
        
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
  

-- | Add an octave to a pitched value.  
addOve  :: Pitched a => a -> Int -> a
e `addOve` o = e `addSemi` (12 * o)

-- | Subtract an octave from a pitched value.
subOve  :: Pitched a => a -> Int -> a
e `subOve` o = e `subSemi` (12 * o)


-- | As per 'semitoneDisplacement' but just return the distance.
semitoneDistance    :: Pitched a => a -> a -> Int
semitoneDistance    = snd `dyap` semitoneDisplacement

-- | As per 'semitoneDisplacement' but just return the direction.   
semitoneDirection   :: Pitched a => a -> a -> Direction
semitoneDirection   = fst `dyap` semitoneDisplacement  



--------------------------------------------------------------------------------
-- Instances


    
instance Pitched Pitch where
  p `addSemi` i = p + (fromInteger $ fromIntegral i)
  p `subSemi` i = p - (fromInteger $ fromIntegral i)
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
  addSemi l i = toEnum $ (fromEnum l) + i
  subSemi l i = toEnum $ (fromEnum l) - i
  
  semitoneDisplacement l l' = 
    let d = countTo succ l l'
    in if (d > 6) then (Downwards, 12 - d) else (Upwards, abs d)


      
--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------
{-
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

  
instance Enum PitchLabel where 
  fromEnum (PitchLabel l a) = mod12 $ fn l + semitones a
    where fn C = 0
          fn D = 2
          fn E = 4
          fn F = 5
          fn G = 7
          fn A = 9
          fn B = 11

  
  toEnum 0   = PitchLabel C Nat
  toEnum 1   = PitchLabel C Sharp
  toEnum 2   = PitchLabel D Nat
  toEnum 3   = PitchLabel D Sharp
  toEnum 4   = PitchLabel E Nat
  toEnum 5   = PitchLabel F Nat
  toEnum 6   = PitchLabel F Sharp
  toEnum 7   = PitchLabel G Nat
  toEnum 8   = PitchLabel G Sharp
  toEnum 9   = PitchLabel A Nat
  toEnum 10  = PitchLabel A Sharp
  toEnum 11  = PitchLabel B Nat

  toEnum i  = toEnum $ mod12 i
  

-}


--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------

{- 
instance Ord Accidental where
  compare a a' = fromEnum a `compare` fromEnum a' 

instance Ord PitchLabel where
  compare a a' = fromEnum a `compare` fromEnum a' 

 
instance Ord Pitch where
  compare (Pitch _ o s c) (Pitch _ o' s' c') = a `compare` b
    where 
      (sc,cc)   = explode100 c
      (sc',cc') = explode100 c'
      a         = octaveToCents o + semitonesToCents (s + sc) + cc
      b         = octaveToCents o' + semitonesToCents (s' + sc') + cc'
    
-}


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
  

--------------------------------------------------------------------------------
-- Deco instances
--------------------------------------------------------------------------------


instance Deco Pitch where 
  deco = decoPitch

-- | Parsec parser for 'Pitch'.
decoPitch :: Parser Pitch
decoPitch = fn <$> deco <*> option 4 positiveInt <*> option 0 signedInt
  where
    fn pl o c = pitch pl o                     
                       
                       
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
  affi (Pitch l a o) = affi l . affi a . shows o
    
    
instance Affi PitchLabel where 
    affi (PitchLabel l a) = affi l . affi a
        
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

pnat l    = PitchLabel l Nat
psharp l  = PitchLabel l Sharp
pflat l   = PitchLabel l Flat

c_natural   :: PitchLabel
c_natural   = pnat C

d_natural   :: PitchLabel
d_natural   = pnat D

e_natural   :: PitchLabel
e_natural   = pnat E

f_natural   :: PitchLabel
f_natural   = pnat F

g_natural   :: PitchLabel 
g_natural   = pnat G

a_natural   :: PitchLabel
a_natural   = pnat A

b_natural   :: PitchLabel 
b_natural   = pnat B

c_sharp     :: PitchLabel
c_sharp     = psharp C
 
d_flat      :: PitchLabel  
d_flat      = pflat D

d_sharp     :: PitchLabel 
d_sharp     = psharp D

e_flat      :: PitchLabel  
e_flat      = pflat E

f_sharp     :: PitchLabel 
f_sharp     = psharp F

g_flat      :: PitchLabel  
g_flat      = pflat G

g_sharp     :: PitchLabel 
g_sharp     = psharp G

a_flat      :: PitchLabel  
a_flat      = pflat A

a_sharp     :: PitchLabel 
a_sharp     = psharp A

b_flat      :: PitchLabel
b_flat      = pflat B


pchNat n o    = pitch (naturalNote n) o
pchSharp n o  = pitch (sharpNote n) o
pchFlat n o   = pitch (flatNote n) o

