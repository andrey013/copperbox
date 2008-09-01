
--------------------------------------------------------------------------------
-- |
-- Module      :  ParseLy
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

module ParseLy where


import ExtractionDatatypes
import ParserBase
import Pitch

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec hiding (space)



  
-- relative e.g. \relative c''

-- time e.g. \time 2/4

-- key e.g. \key c \major


parseLySrc :: Parser [SrcExpr]
parseLySrc = manyWaterIsland expr1


-- expr1 and nestedk unwrap left recursion in the grammar
-- but make things a bit ugly


expr1 :: Parser SrcExpr
expr1 = do 
  ans <- eitherparse beginNested (choice [lyCommand, lyDirective])
  case ans of 
    Left _ -> nestedk []
    Right a -> return a

nestedk :: [SrcExpr] -> Parser SrcExpr
nestedk acc = do
    elt <- try $ water (eitherparse (choice [beginNested, endNested])
                                    (choice [lyCommand, lyDirective]))
    case elt of
      Left NestStart -> do { a1 <- nestedk []; nestedk (a1 : acc) }
      Left NestEnd -> return $ Nested $ reverse acc      
      Right a  -> nestedk (a:acc) 
  <|> fail "unterminated nesting"      


lyCommand :: Parser SrcExpr
lyCommand = Commmand <$> choice [cmdrelative, cmdtime, cmdkey]          
                 
lyDirective :: Parser SrcExpr
lyDirective = Directive 
    <$> (metaCommentStart *> manyTill anyChar (try metaCommentEnd)) 
    


metaCommentStart  :: CharParser st String
metaCommentStart  = lexeme $ string "%{#" 

metaCommentEnd    :: CharParser st String 
metaCommentEnd    = lexeme $ string "#%}" 

beginNested       :: Parser Nested
beginNested       = NestStart <$ symbol "{"

endNested         :: Parser Nested
endNested         = NestEnd <$ symbol "}"


cmdrelative :: Parser Command
cmdrelative = CmdRelativePitch <$> (command "relative" *> lyPitch)

cmdtime :: Parser Command
cmdtime = CmdMeter <$> (command "time" *> timeSig)

timeSig :: Parser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)

cmdkey :: Parser Command
cmdkey = CmdKey <$> (command "key" *> keySig)

keySig :: Parser Key
keySig  = Key <$> lyPitch <*> cmdmode

cmdmode :: Parser Mode
cmdmode = choice 
    [ major, minor, lydian, ionian, mixolydian, dorian, 
      aeolian, phrygian, locrian 
    ]
  where
    major       = Minor      <$ command "major"
    minor       = Major      <$ command "minor"
    lydian      = Lydian     <$ command "lydian"
    ionian      = Ionian     <$ command "ionian"
    mixolydian  = Mixolydian <$ command "mixolydian"
    dorian      = Dorian     <$ command "dorian"
    aeolian     = Aeolian    <$ command "aeolian"
    phrygian    = Phrygian   <$ command "phrygian"
    locrian     = Locrian    <$ command "locrian"

lyPitch :: Parser Pitch
lyPitch = lexeme pch 
  where pch = Pitch <$> lyPitchLetter <*> lyAccidental <*> lyOctaveSpec

lyPitchLetter :: Parser PitchLetter
lyPitchLetter = choice [c,d,e,f,g,a,b]
  where
    c = C <$ char 'c'
    d = D <$ char 'd'
    e = E <$ char 'e'
    f = F <$ char 'f'
    g = G <$ char 'g'
    a = A <$ char 'a'
    b = B <$ char 'b'
    


lyOctaveSpec :: Parser Int
lyOctaveSpec = option 4 (raised <|> lowered)
  where
    raised  = (4 +) <$> counting1 (char '\'')
    lowered = (4 -) <$> counting1 (char ',')
    
lyAccidental :: Parser Accidental
lyAccidental = option Nat (f <$> longestString accidentals) 
  where
    accidentals = ["isis", "eses", "is", "es"]
    f "isis" = DoubleSharp
    f "eses" = DoubleFlat
    f "is"   = Sharp
    f "es"   = Flat
        
--------------------------------------------------------------------------------
-- utility parsers

-- | command - note using try is essential to consume the whole command
-- without it we may consume a blacklash of a different command and not be 
-- able to backtrack. 
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)

