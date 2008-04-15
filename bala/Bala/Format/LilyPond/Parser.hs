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




pitch = Pitch <$> pitchLetter <*> optparse octaveSpec



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

type Command = (String,String) -- to improve...


bar :: Parser Command
bar = (,) <$> command "bar" <*> doubleQuoted barMark

version :: Parser Command
version = (,) <$> command "version" <*> doubleQuoted deweyNumber
  where deweyNumber = many1 $ oneOf "0123456789."

header :: Parser Command
header = (,) <$> command "header" <*> bracedWater 
  
        


--------------------------------------------------------------------------------
-- other
--------------------------------------------------------------------------------

bracedWater :: Parser String
bracedWater = fst <$> (braceOpen *> collectWater braceClose)

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



       