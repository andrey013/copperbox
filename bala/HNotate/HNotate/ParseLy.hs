
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParseLy
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParseLy - parse Lilypond (.ly) files for template holes.
--
--------------------------------------------------------------------------------

module HNotate.ParseLy where

import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.PreprocessTemplate (lyPrePro)
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Data.Monoid
import Data.Ratio
import Data.Sequence ( (|>) )
import Text.ParserCombinators.Parsec hiding (space)




lyTextualView :: FilePath -> IO (Either ParseError TextualView)
lyTextualView path = parseFromFileState (textView start end) path 0
  where
    start = lexeme $ string "%{#"
    end   = lexeme $ string "#%}"

lyExpressionView :: FilePath -> IO (Either ParseError ExprView)
lyExpressionView = twoPass lyPrePro lyExprView
              
lyExprView :: StParser ExprView
lyExprView = exprView updateWithLyCommands

--                   

updateWithLyCommands :: StParser (Env -> Env)
updateWithLyCommands = choice $ [ cmdRelative, cmdTime, cmdKey, cmdCadenzaOn, 
   cmdCadenzaOff, cmdPartial ]    
                     
                     
cmdRelative :: StParser (Env -> Env)
cmdRelative = set_relative_pitch <$> (command "relative" *> lyPitch)

cmdTime :: StParser (Env -> Env)
cmdTime = set_current_meter <$> (command "time" *> timeSig)

cmdKey :: StParser (Env -> Env)
cmdKey = set_current_key <$> (command "key" *> keySig)

cmdMode :: StParser Mode
cmdMode = choice 
    [ major, minor, lydian, ionian, mixolydian, dorian, 
      aeolian, phrygian, locrian 
    ]
  where
    major       = Minor      <$ command "major"
    minor       = Major      <$ command "major"
    lydian      = Lydian     <$ command "lydian"
    ionian      = Ionian     <$ command "ionian"
    mixolydian  = Mixolydian <$ command "mixolydian"
    dorian      = Dorian     <$ command "dorian"
    aeolian     = Aeolian    <$ command "aeolian"
    phrygian    = Phrygian   <$ command "phrygian"
    locrian     = Locrian    <$ command "locrian"

cmdCadenzaOn    :: StParser (Env -> Env)
cmdCadenzaOn    = (set_cadenza True)  <$ command "cadenzaOn"

cmdCadenzaOff   :: StParser (Env -> Env)
cmdCadenzaOff   = (set_cadenza False) <$ command "cadenzaOff"

cmdPartial      :: StParser (Env -> Env)
cmdPartial      = set_partial_measure <$ command "partial" <*> lyDuration



    
    
timeSig :: StParser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


keySig :: StParser Key
keySig  = Key <$> lyPitch <*> cmdMode


lyPitch :: StParser Pitch
lyPitch = lexeme pch 
  where pch = Pitch <$> lyPitchLetter <*> lyAccidental <*> lyOctaveSpec

lyPitchLetter :: StParser PitchLetter
lyPitchLetter = choice [c,d,e,f,g,a,b]
  where
    c = C <$ char 'c'
    d = D <$ char 'd'
    e = E <$ char 'e'
    f = F <$ char 'f'
    g = G <$ char 'g'
    a = A <$ char 'a'
    b = B <$ char 'b'
    

--  << \relative c >> gives c below middle c i.e octave 3
--  << \relative c' >> gives middle c i.e octave 4
lyOctaveSpec :: StParser Int
lyOctaveSpec = option 4 (raised <|> lowered)
  where
    raised  = (3 +) <$> counting1 (char '\'')
    lowered = (3 -) <$> counting1 (char ',')
    
lyAccidental :: StParser Accidental
lyAccidental = option Nat (f <$> longestString accidentals) 
  where
    accidentals = ["isis", "eses", "is", "es"]
    f "isis" = DoubleSharp
    f "eses" = DoubleFlat
    f "is"   = Sharp
    f "es"   = Flat


lyDuration :: StParser Duration
lyDuration = 
    build <$> rootDuration <*> (counting $ symbol ".") 
                           <*> optparse (symbol "*" *> fractionalPart)
  where
    build d dc Nothing  = dotconst d dc
    build d dc (Just r) = (dotconst d dc) * r
    fractionalPart      = (\n d -> durationR (n%d)) 
                              <$> int <*> option 1 (symbol "/" *> int)

rootDuration :: StParser Duration
rootDuration = choice [pBreve, pLonga, pNumericDuration]
  where
    pBreve            = breve <$ command "breve"   
    pLonga            = longa <$ command "longa"
    pNumericDuration  = (\i -> Duration (1%i) 0) <$> int 
    

    
    
--------------------------------------------------------------------------------
-- utility parsers

-- | command - note using try is essential to consume the whole command
-- without it we may consume a blacklash of a different command and not be 
-- able to backtrack. 
command     :: String -> CharParser st String
command ss  = lexeme $ try $ string ('\\':ss)

