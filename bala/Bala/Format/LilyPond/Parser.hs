--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parser for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Parser where

import Bala.Base.BaseExtra
import Bala.Format.LilyPond.Datatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad



import Text.ParserCombinators.Parsec hiding (space)


rest :: Parser Rest
rest = Rest <$> (char 'r' *> duration)

duration :: Parser Int
duration = int



pitch :: Parser Pitch
pitch = Pitch <$> pitchLetter <*> optparse octaveSpec

-- | temporary expedience
pitchstring :: Parser String
pitchstring = show <$> pitch


octaveSpec :: Parser OctaveSpec
octaveSpec = raised <|> lowered
  where
    raised  = Raised  <$> counting1 (char '\'')
    lowered = Lowered <$> counting1 (char ',')
    
accidental :: Parser Accidental
accidental = choice [ss, ff, s, f] 
  where
    ss = fn DoubleSharp "isis"
    ff = fn DoubleFlat  "eses"
    s  = fn Sharp       "is"
    f  = fn Flat        "es"
    
    fn cnstr str = cnstr <$ lexeme (string str)
    
microTone :: Parser MicroTone
microTone = choice [hs, hf] 
  where
    hs = fn HalfSharp "ih"
    hf = fn HalfFlat  "eh"
    
    fn cnstr str = cnstr <$ lexeme (string str)  
    
stringMark :: Parser String
stringMark = char '^' *> stringLiteral

chord :: Parser [Pitch]
chord = angles (sepBy1 pitch whiteSpace)

--------------------------------------------------------------------------------
-- commands
--------------------------------------------------------------------------------



command0 :: (Command) -> String -> Parser Command
command0 f name = f <$ command name

nullaryCommand :: String -> Parser Command
nullaryCommand name = command0 (NullaryCommand name) name

command1 :: (a -> Command) -> String -> Parser a -> Parser Command
command1 f name p = f <$> (command name *> lexeme p)

unaryCommand :: String -> Parser String -> Parser Command
unaryCommand name = command1 (UnaryCommand name) name

command2 :: (a -> b -> Command) -> String -> Parser a -> Parser b -> Parser Command
command2 f name p p' = f <$> (command name *> lexeme p) <*> lexeme p'

binaryCommand :: String -> Parser String -> Parser String -> Parser Command
binaryCommand name = command2 (BinaryCommand name) name 



bar :: Parser Command
bar = unaryCommand "bar" (doubleQuoted barMark)


version :: Parser Command
version = command1 CmdVersion "version" (doubleQuoted deweyNumber)
  where deweyNumber = sepBy1 int (char '.')

header :: Parser Command
header = unaryCommand "header" bracedWater 


-- | TODO rendering pitch back to string BAD! 
key :: Parser Command
key = command2 CmdKey "key" pitch (keytype <|> mode)
 

keytype :: Parser Command
keytype = nullaryCommand "major" <|> nullaryCommand "minor"

mode :: Parser Command
mode = choice $ map nullaryCommand [ "ionian", "locrian",
          "aeolian", "mixolydian", "lydian", "phrygian", "dorian" ]
  
        
clef :: Parser Command
clef = unaryCommand "clef" clefname 
  where
    clefname = longestString clefs <|> quotedclef
    
    quotedclef = doubleQuoted $ many1 (alphaNum <|> oneOf "_^")
     
    clefs = ["treble", "violin", "G", "G2",
             "alto", "C", "tenor", "bass", "F",            
             "french", "soprano", "mezzosoprano",
             "baritone", "varbaritone", "subbass",
             "percussion", "tab"
             ]

dynamic :: Parser Command
dynamic = NullaryCommand <$> (longestString dynamic_mark)  
  where
    dynamic_mark = ["ppppp", "pppp", "ppp", "pp", "p", 
                    "mp", "mf", "f", "ff", "fff", "ffff", 
                    "fp", "sf", "sff", "sp", "spp", "sfz", "rfz"
                    ]

espressivo :: Parser Command
espressivo = nullaryCommand "espressivo"

breath :: Parser Command
breath = nullaryCommand "breath"

glissando :: Parser Command
glissando = nullaryCommand "glissando"

arpeggio :: Parser Command
arpeggio = nullaryCommand "arpeggio"

arpeggioBracket :: Parser Command
arpeggioBracket = nullaryCommand "arpeggioBracket"

arpeggioUp :: Parser Command
arpeggioUp = nullaryCommand "arpeggioUp"

arpeggioDown :: Parser Command
arpeggioDown = nullaryCommand "arpeggioDown"

arpeggioNeutral :: Parser Command
arpeggioNeutral = nullaryCommand "arpeggioNeutral"

 
time :: Parser Command
time = command1 (uncurry CmdTimeSignature) "time" fraction


partial :: Parser Command
partial = unaryCommand "partial" (show <$> int)

italianGrace :: Parser Command
italianGrace =  nullaryCommand "appoggiatura" 
            <|> nullaryCommand "acciaccatura"


--------------------------------------------------------------------------------
-- other
--------------------------------------------------------------------------------

bracedWater :: Parser String
bracedWater = fst <$> (braceOpen *> collectWater braceClose)

fraction :: Parser (Int,Int)
fraction = (,) <$> (int <* char '/') <*> int

barMark :: Parser String
barMark = longestString
        [ "unbroken ||:", "broken ||:"
        , ".|.", ":|:", "||", "|.", ".|", "|:", ":|", "|" , ":"] 
        
comment :: Parser ()
comment = char '%' *> text *> lineEnd
  where lineEnd = () <$ choice [char '\r',  char '\n']
        text    = noneOf "\n\r"



        
--------------------------------------------------------------------------------
-- lexer combinators 
--------------------------------------------------------------------------------

command :: String -> CharParser st String
command ss = lexeme $ (:) <$> char '\\' <*> string ss


        
        

tie :: CharParser st Char
tie = char '~'


doubleQuote :: CharParser st Char
doubleQuote = char '"'

singleQuote :: CharParser st Char
singleQuote = char '\''


pitchLetter :: CharParser st Char
pitchLetter = oneOf "cdefgab"

doubleAngleOpen :: CharParser st String
doubleAngleOpen = string "<<"

doubleAngleClose :: CharParser st String
doubleAngleClose = string ">>"

angleOpen :: CharParser st Char
angleOpen = char '<'

angleClose :: CharParser st Char
angleClose = char '>'

beamOpen :: CharParser st Char
beamOpen = char '['

beamClose :: CharParser st Char
beamClose = char ']'

braceOpen :: CharParser st Char
braceOpen = char '{'

braceClose :: CharParser st Char
braceClose = char '}'



       