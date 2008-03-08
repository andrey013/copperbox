

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representations
-- |
--------------------------------------------------------------------------------


module Bala.Base.Chord where

import Bala.Base.PitchRep
import Bala.Base.Interval
import Bala.Base.PitchClass
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Pos

-- a chord is like a pitch class set, but with possible intervals > 12
data Chord = Chord {
    chord_root       :: Pitch,
    semitone_interval_pattern :: [Int]
  }
  deriving (Eq,Show)


data RomanChord = RomanChord {
    root_alteration   :: Maybe Alteration, 
    scale_degree      :: Int,
    chord_quality     :: ChordQuality,
    chord_variation   :: Maybe Variation,    
    inversion_label   :: InversionLabel
  }
  deriving (Eq)

data Alteration = RSharp | RFlat
  deriving (Eq) 
  
data ChordQuality = RMajor | RMinor
  deriving (Eq)

data Variation = Dim | Aug
  deriving (Eq)
  
data InversionLabel = IRoot | IFirst | ISecond | IThird
  deriving (Eq)

data GuitarChord = GuitarChord {
    guiord_pitch :: PitchLetter,
    guiord_type  :: LabelledChord
  }

data LabelledChord 
  = -- Major   
    Maj' | Maj6 | Maj7 | Maj9 | Maj11 | Maj13 | MajAdd9 | Maj6_9
    -- Minor 
  | Min' | Min6 | Min7 | Min9 | Min11 | Min13 | MinAdd9 | Min6_9
  | MinMaj7 | MinMaj9 
    -- Dominant
  | Dom7 | Dom9 | Dom11 | Dom13  
  -- Diminished
  | Dim' | Dim7 | HalfDim7
  -- Augmented
  | Aug' | Aug7
  -- Suspended 
  | Sus2 | Sus4 | Sus7
  deriving (Eq)


                   
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

buildChord :: Pitch -> IntervalPattern -> Chord
buildChord a (IntervalPattern xs) = Chord a (scanl shiftyPlus (fixedPitch a) xs)

-- these would be better as scale degrees (e.g. major triad 1-3-5), so 
-- we need a new parser
major_triad_pattern = IntervalPattern [5,4]
minor_triad_pattern = IntervalPattern [4,5]
diminished_pattern  = IntervalPattern [4,4]
augmented_pattern   = IntervalPattern [5,5]



pitches :: Chord -> [Pitch]
pitches (Chord root xs) = map (addSemi root) xs
  

-- | Build a triad from a roman chord (within a scale ?)
triad :: RomanChord -> () -> Chord
triad rc@(RomanChord {scale_degree=d}) () = buildChord (tstart d) (tip rc)
  where
    tstart :: Int -> Pitch
    tstart 1 = read "C4"
    tstart 2 = read "D4"
    tstart 3 = read "E4"
    tstart 4 = read "F4"
    tstart 5 = read "G4"
    tstart 6 = read "A4"
    tstart 7 = read "B4"

-- triad interval pattern
tip :: RomanChord -> IntervalPattern
tip (RomanChord {chord_quality=qual,chord_variation=var} ) = 
    alter (base_pattern qual) var 
  where 
    base_pattern RMajor = IntervalPattern [5,4]
    base_pattern RMinor = IntervalPattern [4,5]
    
    alter pat opt = maybe pat (variation pat) opt
    variation (IntervalPattern [a,b]) Dim = IntervalPattern [a, b - 1]
    variation (IntervalPattern [a,b]) Aug = IntervalPattern [a, b + 1]
    
    
    
--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read RomanChord where 
  readsPrec _ s = readsParsec romanChord s
  
  
romanChord :: Parser RomanChord  
romanChord = fn <$> optparse alteration 
                <*> chordNumeral 
                <*> optparse variation
                <*> option IRoot inversionLabel    
  where fn a (b,c) d e = RomanChord a b c d e

  
alteration = sharp <|> flat
  where
    sharp = RSharp <$ char '#'
    flat  = RFlat  <$ char 'b' 
  



    
chordNumeral = lowerNumeral <|> upperNumeral

lowerNumeral :: Parser (Int, ChordQuality)
lowerNumeral = romanNumeral RMinor ('i', 'v', 'x')

upperNumeral :: Parser (Int, ChordQuality)
upperNumeral = romanNumeral RMajor ('I', 'V', 'X')


romanNumeral :: a -> (Char,Char,Char) -> Parser (Int,a)
romanNumeral a (i',v',x') = tryDigit >>= kont 0 >>= valid
  where
    valid 0 = fail "parse error romanNumeral"
    valid i = return (i,a)

    oI = maybe False ((==)  i')
    oV = maybe False ((==)  v')
    oX = maybe False ((==)  x')
    tryDigit :: Parser (Maybe Char)
    tryDigit = optOneOf [i',v',x']
    
    kont i Nothing      = return i  
    kont 0 od | oI od   = tryDigit >>= kont 1
    kont 0 od | oV od   = tryDigit >>= kont 5
    kont 0 od | oX od   = tryDigit >>= kont 10
    kont 1 od | oV od   = return 4
    kont 1 od | oX od   = return 9
    kont i od | oI od   = kontI i 
    
    
    kontI i | i >= 1  && i < 4    = tryDigit >>= kont (i+1)
            | i >= 5  && i < 8    = tryDigit >>= kont (i+1)
            | i >= 10 && i < 14   = tryDigit >>= kont (i+1)
            | otherwise           = return i
            
variation :: Parser Variation
variation = choice [dim,aug]
  where  
    dim = Dim <$ char 'o' 
    aug = Aug <$ char '+'
    
inversionLabel :: Parser InversionLabel
inversionLabel = choice [root,first,second,third]
  where
    root    = IRoot   <$ string "(a)"
    first   = IFirst  <$ char 'b'
    second  = ISecond <$ char 'c'
    third   = IThird  <$ char 'd'


instance Read GuitarChord where 
  readsPrec _ s = readsParsec readGuitarChord s
  
readGuitarChord :: Parser GuitarChord
readGuitarChord = GuitarChord <$> readPitchLetter <*> withLex lexChord

withLex :: (String -> Parser (a, String)) -> Parser a
withLex f = do
  xs <- getInput
  (a,rest) <- f xs
  setInput rest
  return a
  

updatePosWith :: String -> Parser ()
updatePosWith s = do
  pos <- getPosition
  setPosition $ updatePosString pos s

continue :: String -> Parser a -> Parser a 
continue s f = updatePosWith s >> f

lexChord :: String -> Parser (LabelledChord, String)
lexChord ('m':'a':'j':xs) = "maj"     `continue` (lexMaj xs)
lexChord ('m':'a':xs)     = "ma"      `continue` (lexMaj xs)
lexChord ('M':xs)         = "M"       `continue` (lexMaj xs)
lexChord ('a':'d':'d':'9':xs) =
                            "add9"    `continue` return (MajAdd9,xs)
lexChord ('/':'9':xs)     = "/9"      `continue` return (MajAdd9,xs)
lexChord ('6':'/':'9':xs) = "6/9"     `continue` return (Maj6_9,xs)
lexChord ('6':xs)         = "6"       `continue` return (Maj6,xs)
lexChord ('7':xs)         = "7"       `continue` (lex7 xs)
lexChord ('9':'/':'6':xs) = "9/6"     `continue` return (Maj6_9,xs)
lexChord ('9':xs)         = "9"       `continue` return (Dom9, xs)
lexChord ('1':'1':xs)     = "11"      `continue` return (Dom11, xs)
lexChord ('1':'3':xs)     = "13"      `continue` return (Dom13, xs)

lexChord ('m':'i':'n':xs) = "min"     `continue` (lexMin xs)
lexChord ('m':'i':xs)     = "mi"      `continue` (lexMin xs)
lexChord ('m':xs)         = "m"       `continue` (lexMin xs)
lexChord ('+':'5':xs)     = "+"       `continue` return (Aug',xs)
lexChord ('+':xs)         = "+"       `continue` return (Aug',xs)
lexChord ('-':xs)         = "-"       `continue` return (Min',xs)

lexChord ('d':'i':'m':xs) = "dim"     `continue` (lexDim xs)
lexChord ('a':'u':'g':xs) = "aug"     `continue` (lexAug xs)

lexChord ('s':'u':'s':xs) = "sus"     `continue` (lexSus xs)

lexChord ('j':xs)         = "j"       `continue` (lexj xs)

lexChord _                = fail "malformed chord"


lexMaj ('1':'3':xs)       = "13"      `continue` return (Maj13,xs)
lexMaj ('1':'1':xs)       = "11"      `continue` return (Maj11,xs)
lexMaj ('9':xs)           = "9"       `continue` return (Maj9,xs)
lexMaj ('7':xs)           = "7"       `continue` return (Maj7,xs)
lexMaj ('6':xs)           = "6"       `continue` return (Maj6,xs)
lexMaj  xs                = return (Maj',xs)


lexMin ('6':'/':'9':xs)   = "6/9"     `continue` return (Min6_9,xs)
lexMin ('9':'/':'6':xs)   = "9/6"     `continue` return (Min6_9,xs)
lexMin ('1':'3':xs)       = "13"      `continue` return (Min13,xs)
lexMin ('1':'1':xs)       = "11"      `continue` return (Min11,xs)
lexMin ('9':xs)           = "9"       `continue` return (Min9,xs)
lexMin ('7':'-':'5':xs)   = "7-5"     `continue` return (HalfDim7,xs)
lexMin ('7':'b':'5':xs)   = "7b5"     `continue` return (HalfDim7,xs)
lexMin ('7':xs)           = "7"       `continue` return (Min7,xs)
lexMin ('6':xs)           = "6"       `continue` return (Min6,xs)

lexMin ('(':'m':'a':'j':'7':')':xs)       
                          = "(maj7)"  `continue` return (MinMaj7,xs)
lexMin ('M':'7':xs)       = "M7"      `continue` return (MinMaj7,xs)
lexMin ('(':'m':'a':'j':'9':')':xs)       
                          = "(maj9)"  `continue` return (MinMaj9,xs)
lexMin ('M':'9':xs)       = "M9"      `continue` return (MinMaj9,xs)

lexMin ('(':'a':'d':'d':'9':')':xs)       
                          = "(add9)"  `continue` return (MinAdd9,xs)
lexMin ('/':'9':xs)       = "/9"      `continue` return (MinAdd9,xs)

lexMin  xs                = return (Min',xs)

lexDim ('7':xs)           = "7"       `continue` return (Dim7,xs)
lexDim xs                 = return (Dim',xs)

lexAug ('7':xs)           = "7"       `continue` return (Aug7,xs)
lexAug xs                 = return (Aug',xs)

lexSus ('2':xs)           = "2"       `continue` return (Sus2,xs)
lexSus ('4':xs)           = "4"       `continue` return (Sus4,xs)
lexSus xs                 = return (Sus4,xs)

-- prefix (7) either a suspended 7, and augmented 7 (using + or #) or a dominant 7
lex7 ('s':'u':'s':'4':xs) = "sus4"    `continue` return (Sus7,xs)
lex7 ('s':'u':'s':xs)     = "sus"     `continue` return (Sus7,xs)
lex7 ('#':'5':xs)         = "#5"      `continue` return (Aug7,xs)
lex7 ('+':'5':xs)         = "+5"      `continue` return (Aug7,xs)
lex7 xs                   = return (Dom7, xs)


lexj ('1':'3':xs)         = "13"      `continue` return (Maj13,xs)
lexj ('1':'1':xs)         = "11"      `continue` return (Maj11,xs)
lexj ('9':xs)             = "9"       `continue` return (Maj9,xs)
lexj ('7':xs)             = "7"       `continue` return (Maj7,xs)
lexj _                    = fail "j... expecting 7,9,11,13"

  
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

    
instance Show RomanChord where
  showsPrec _ (RomanChord oa i q v inv) = 
    showOpt oa . showNumeral i q . showOpt v . shows inv

showNumeral i RMinor = showLowerNumeral i
showNumeral i RMajor = showUpperNumeral i

showLowerNumeral 1  = showString "i"
showLowerNumeral 2  = showString "ii"
showLowerNumeral 3  = showString "iii"
showLowerNumeral 4  = showString "iv"
showLowerNumeral 5  = showString "v"
showLowerNumeral 6  = showString "vi"
showLowerNumeral 7  = showString "vii"
showLowerNumeral 8  = showString "viii"
showLowerNumeral 9  = showString "ix"
showLowerNumeral _  = showString "_"

showUpperNumeral 1  = showString "I"
showUpperNumeral 2  = showString "II"
showUpperNumeral 3  = showString "III"
showUpperNumeral 4  = showString "IV"
showUpperNumeral 5  = showString "V"
showUpperNumeral 6  = showString "VI"
showUpperNumeral 7  = showString "VII"
showUpperNumeral 8  = showString "VIII"
showUpperNumeral 9  = showString "IX"
showUpperNumeral _  = showString "_"

instance Show Alteration where
  showsPrec _ RSharp = showChar '#'
  showsPrec _ RFlat  = showChar 'b'

  

instance Show Variation where
  showsPrec _ Dim = showChar 'o'
  showsPrec _ Aug = showChar '+'

  
instance Show InversionLabel where
  showsPrec _ IRoot   = id
  showsPrec _ IFirst  = showChar 'b'     
  showsPrec _ ISecond = showChar 'c'
  showsPrec _ IThird  = showChar 'd'

instance Show GuitarChord where
  showsPrec _ (GuitarChord p q) = 
      shows p . shows q


instance Show LabelledChord where
  showsPrec _ Maj'      = showString "maj"
  showsPrec _ Maj6      = showString "maj6"
  showsPrec _ Maj7      = showString "maj7"
  showsPrec _ Maj9      = showString "maj9"
  showsPrec _ Maj11     = showString "maj11"
  showsPrec _ Maj13     = showString "maj13"
  showsPrec _ MajAdd9   = showString "/9"
  showsPrec _ Maj6_9    = showString "6/9"
  showsPrec _ Min'      = showString "min" 
  showsPrec _ Min6      = showString "min6"
  showsPrec _ Min7      = showString "min7"
  showsPrec _ Min9      = showString "min9"
  showsPrec _ Min11     = showString "min11" 
  showsPrec _ Min13     = showString "min13"
  showsPrec _ MinAdd9   = showString "m/9"
  showsPrec _ Min6_9    = showString "m6/9"
  showsPrec _ MinMaj7   = showString "mM7" 
  showsPrec _ MinMaj9   = showString "mM9"
  showsPrec _ Dom7      = showString "7"
  showsPrec _ Dom9      = showString "9"
  showsPrec _ Dom11     = showString "11"
  showsPrec _ Dom13     = showString "13"
  showsPrec _ Dim'      = showString "dim"
  showsPrec _ Dim7      = showString "dim7"
  showsPrec _ HalfDim7  = showString "m7b5"
  showsPrec _ Aug'      = showString "aug"
  showsPrec _ Aug7      = showString "aug7"
  showsPrec _ Sus2      = showString "sus2"
  showsPrec _ Sus4      = showString "sus"
  showsPrec _ Sus7      = showString "7sus"
 