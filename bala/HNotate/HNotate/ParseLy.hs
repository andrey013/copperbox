
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
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Data.Monoid
import Data.Ratio
import Data.Sequence ( (|>) )
import Text.ParserCombinators.Parsec hiding (space)


lyPIV :: FilePath -> IO (Either ParseError PIV)
lyPIV filepath = parseFromFileState interpretableView filepath 0


lySPV :: FilePath -> IO (Either ParseError SPV)
lySPV filepath = parseFromFileState textView filepath 0


textView :: StParser SPV
textView = collect mempty
  where
    collect se = do 
      txt    <- upTo (eitherparse eof (lexeme metaCommentStart))
      at_end <- option False (True <$ eof)
      case at_end of
        True -> return $ SPV (se `add` txt)
        False -> do { mark <- metamarkK
                    ; collect $ (se `add` txt) |> mark }  
    
    add se "" = se
    add se ss = se |> (SourceText ss)       

metamarkK = MetaMark 
    <$> incrCount <*> sourcePosition <*> (metadirective <* metaCommentEnd) 
    
        
-- relative e.g. \relative c''

-- time e.g. \time 2/4

-- key e.g. \key c \major


interpretableView :: StParser PIV
interpretableView = PIV <$> manyWaterIsland expr1


-- expr1 and nestedk unwrap the left recursion in the grammar
-- but make things a bit ugly


expr1 :: StParser ScoreElement
expr1 = do 
  ans <- eitherparse beginNested (choice [lyCommand, lyDirective])
  case ans of 
    Left _ -> nestedk []
    Right a -> return a

nestedk :: [ScoreElement] -> StParser ScoreElement
nestedk acc = do
    elt <- try $ water (eitherparse (choice [beginNested, endNested])
                                    (choice [lyCommand, lyDirective]))
    case elt of
      Left NestStart -> do { a1 <- nestedk []; nestedk (a1 : acc) }
      Left NestEnd -> return $ Nested $ reverse acc      
      Right a  -> nestedk (a:acc) 
  <|> fail "unterminated nesting"      


lyCommand :: StParser ScoreElement
lyCommand = Command <$> choice 
  [ cmdRelative, cmdTime, cmdKey, cmdCadenzaOn, cmdCadenzaOff, cmdPartial ]          
                 
lyDirective :: StParser ScoreElement
lyDirective = Directive 
    <$> incrCount <*> ((lexeme metaCommentStart) *> metadirective) 
                  <*  (lexeme metaCommentEnd)


    
metadirective :: StParser MetaDirective
metadirective = metaoutput

metaoutput :: StParser MetaDirective
metaoutput = MetaOutput <$> (identifier <* colon) <*> identifier

metaCommentStart  :: CharParser st String
metaCommentStart  = string "%{#" 

metaCommentEnd    :: CharParser st String 
metaCommentEnd    = string "#%}" 

beginNested       :: StParser Nested
beginNested       = NestStart <$ symbol "{"

endNested         :: StParser Nested
endNested         = NestEnd <$ symbol "}"


cmdRelative :: StParser Command
cmdRelative = CmdRelativePitch <$> (command "relative" *> lyPitch)

cmdTime :: StParser Command
cmdTime = CmdMeter <$> (command "time" *> timeSig)

cmdKey :: StParser Command
cmdKey = CmdKey <$> (command "key" *> keySig)

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

cmdCadenzaOn    :: StParser Command
cmdCadenzaOn    = CmdCadenzaOn  <$ command "cadenzaOn"

cmdCadenzaOff   :: StParser Command
cmdCadenzaOff   = CmdCadenzaOff <$ command "cadenzaOff"

cmdPartial      :: StParser Command
cmdPartial      = CmdPartialMeasure <$ command "partial" <*> lyDuration


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
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)

