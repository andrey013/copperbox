

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.ChordSynonyms
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative chord representations used for printing and parsing only
--
--------------------------------------------------------------------------------

module Bala.Base.ChordSynonyms where

import Bala.Base.BaseExtra
import Bala.Base.PitchRep
import Bala.Base.PitchOps
import Bala.Base.Chord

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Pos


-- | Roman chords are only for parsing roman notation.
data RomanChord = RomanChord {
    root_alteration   :: Maybe Alteration, 
    scale_degree      :: Int,
    chord_quality     :: ChordQuality,
    chord_variation   :: Maybe Variation,    
    inversion_label   :: Inversion
  }
  deriving (Eq)

data Alteration = RSharp | RFlat
  deriving (Eq) 
  
data ChordQuality = RMajor | RMinor
  deriving (Eq)

data Variation = Dim | Aug
  deriving (Eq)
  
-- | Jazz / guitar style labelled chords.
data LabelledChord = LabelledChord {
    chord_pitch  :: PitchLetter,
    chord_suffix :: ChordSuffix
  }

data ChordSuffix 
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
  

romanChord :: Parser RomanChord  
romanChord = fn <$> optparse alteration 
                <*> chordNumeral 
                <*> optparse variation
                <*> option RootPosition inversionLabel    
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
    
inversionLabel :: Parser Inversion
inversionLabel = choice [root,first,second,third]
  where
    root    = RootPosition    <$ string "(a)"
    first   = FirstInversion  <$ char 'b'
    second  = SecondInversion <$ char 'c'
    third   = ThirdInversion  <$ char 'd'
    

    
readLabelledChord :: Parser LabelledChord
readLabelledChord = LabelledChord <$> decoPitchLetter <*> withLex lexChordSuffix

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

lexChordSuffix :: String -> Parser (ChordSuffix, String)
lexChordSuffix ('m':'a':'j':xs) = "maj"     `continue` (lexMaj xs)
lexChordSuffix ('m':'a':xs)     = "ma"      `continue` (lexMaj xs)
lexChordSuffix ('M':xs)         = "M"       `continue` (lexMaj xs)
lexChordSuffix ('a':'d':'d':'9':xs) =
                            "add9"    `continue` return (MajAdd9,xs)
lexChordSuffix ('/':'9':xs)     = "/9"      `continue` return (MajAdd9,xs)
lexChordSuffix ('6':'/':'9':xs) = "6/9"     `continue` return (Maj6_9,xs)
lexChordSuffix ('6':xs)         = "6"       `continue` return (Maj6,xs)
lexChordSuffix ('7':xs)         = "7"       `continue` (lex7 xs)
lexChordSuffix ('9':'/':'6':xs) = "9/6"     `continue` return (Maj6_9,xs)
lexChordSuffix ('9':xs)         = "9"       `continue` return (Dom9, xs)
lexChordSuffix ('1':'1':xs)     = "11"      `continue` return (Dom11, xs)
lexChordSuffix ('1':'3':xs)     = "13"      `continue` return (Dom13, xs)

lexChordSuffix ('m':'i':'n':xs) = "min"     `continue` (lexMin xs)
lexChordSuffix ('m':'i':xs)     = "mi"      `continue` (lexMin xs)
lexChordSuffix ('m':xs)         = "m"       `continue` (lexMin xs)
lexChordSuffix ('+':'5':xs)     = "+"       `continue` return (Aug',xs)
lexChordSuffix ('+':xs)         = "+"       `continue` return (Aug',xs)
lexChordSuffix ('-':xs)         = "-"       `continue` return (Min',xs)

lexChordSuffix ('d':'i':'m':xs) = "dim"     `continue` (lexDim xs)
lexChordSuffix ('a':'u':'g':xs) = "aug"     `continue` (lexAug xs)

lexChordSuffix ('s':'u':'s':xs) = "sus"     `continue` (lexSus xs)

lexChordSuffix ('j':xs)         = "j"       `continue` (lexj xs)

lexChordSuffix _                = fail "malformed chord suffix"


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

  
    
    
      