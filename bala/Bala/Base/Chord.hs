

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Triad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Triad representation
-- |
--------------------------------------------------------------------------------


module Bala.Base.Chord where

import Bala.Base.PitchRep
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 


newtype Chord = Chord {unChord :: [Int]}


data RomanChord = RomanChord {
    root_alteration   :: Maybe RootAlteration, 
    scale_degree      :: Int,
    chord_quality     :: ChordQuality,
    chord_variation   :: Maybe Variation,    
    inversion_label   :: InversionLabel
  }
  deriving (Eq)

data RootAlteration = RSharp | RFlat
  deriving (Eq) 
  
data ChordQuality = RMinor | RMajor 
  deriving (Eq)

data Variation = Dim | Aug
  deriving (Eq)
  
data InversionLabel = IRoot | IFirst | ISecond | IThird
  deriving (Eq)


    
  
--------------------------------------------------------------------------------
-- Read instance
--------------------------------------------------------------------------------

instance Read RomanChord where 
  readsPrec _ s = readsParsec romanChord s
  
  
romanChord :: Parser RomanChord  
romanChord = fn <$> optparse rootAlteration 
                <*> chordNumeral 
                <*> optparse variation
                <*> option IRoot inversionLabel    
  where fn a (b,c) d e = RomanChord a b c d e

  
rootAlteration = sharp <|> flat
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
    
--------------------------------------------------------------------------------
-- Show instance
--------------------------------------------------------------------------------

    
instance Show RomanChord where
  showsPrec _ (RomanChord oa i q v inv) = 
    showOpt oa . showNumeral i q . showOpt v . shows inv

showNumeral i RMinor = showLowerNumeral i
showNumeral i RMajor = showUpperNumeral i

showLowerNumeral 1  = showChar   'i'
showLowerNumeral 2  = showString "ii"
showLowerNumeral 3  = showString "iii"
showLowerNumeral 4  = showString "iv"
showLowerNumeral 5  = showChar   'v'
showLowerNumeral 6  = showString "vi"
showLowerNumeral 7  = showString "vii"
showLowerNumeral 8  = showString "viii"
showLowerNumeral 9  = showString "ix"
showLowerNumeral _  = showString "_"

showUpperNumeral 1  = showChar   'I'
showUpperNumeral 2  = showString "II"
showUpperNumeral 3  = showString "III"
showUpperNumeral 4  = showString "IV"
showUpperNumeral 5  = showChar   'V'
showUpperNumeral 6  = showString "VI"
showUpperNumeral 7  = showString "VII"
showUpperNumeral 8  = showString "VIII"
showUpperNumeral 9  = showString "IX"
showUpperNumeral _  = showString "_"

instance Show RootAlteration where
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

  
       