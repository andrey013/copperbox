
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

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Data.Monoid
import Data.Ratio
import Data.Sequence
import Text.ParserCombinators.Parsec hiding (space)


lyExprView_TwoPass :: ExprParser
lyExprView_TwoPass = twoPass preprocessLy parseLyExprs

lyExprView_TwoPass_debug :: ExprParser
lyExprView_TwoPass_debug = twoPass_debug preprocessLy parseLyExprs


--------------------------------------------------------------------------------
-- Preprocess

preprocessLy :: FilePath -> IO (Either ParseError String)
preprocessLy path = either (return . Left) (return . Right . rewriteLyToks) 
                          =<< (parseFromFile lyExtract path)
                          
lyExtract :: Parser (Seq Token)
lyExtract = waterAcc $ choice 
    [ metaComment, lyComment, leftBrace, rightBrace, 
      -- commands
      key, time, relative, partial, cadenzaOn, cadenzaOff
      
    ]


type Leftwards = Maybe () 
braceleft = Just ()

rewriteLyToks :: Seq Token -> String
rewriteLyToks = rewriteTokenStream . cata (:) []


-- first part needs try so we don't consume the "%{" of a normal comment
metaComment :: Parser (TokenF Token)
metaComment = token1 fn <$> 
    ((try $ symbol "%{#") *> manyTill anyChar (try $ symbol "#%}"))
  where
    -- prefix with a hash  
    fn s = '#' : s
                
lyComment :: Parser (TokenF Token)
lyComment = dropToken <$
    (symbol "%{" *> manyTill anyChar (try $ symbol "%}")) 

leftBrace :: Parser (TokenF Token)
leftBrace = beginNest <$ symbol "{"

rightBrace :: Parser (TokenF Token)
rightBrace = endNest <$ symbol "}"

-- commands

key :: Parser (TokenF Token)
key = token3 id id id <$> cmdsymbol "key" <*> nonwhite <*> cmdany


time :: Parser (TokenF Token)
time = token2 id id <$> cmdsymbol "time" <*> nonwhite

relative :: Parser (TokenF Token)
relative = token2 id id <$> cmdsymbol "relative" <*> nonwhite

partial :: Parser (TokenF Token)
partial = token2 id id <$> cmdsymbol "partial" <*> nonwhite

cadenzaOn :: Parser (TokenF Token)
cadenzaOn = token1 id <$> cmdsymbol "cadenzaOn"

cadenzaOff :: Parser (TokenF Token)
cadenzaOff = token1 id <$> cmdsymbol "cadenzaOff"


cmdany :: Parser String
cmdany = (:) <$> char '\\' <*> nonwhite

--------------------------------------------------------------------------------
-- Parse

parseLyExprs :: Parser [Expr]
parseLyExprs = topLevelExprs lyTermParsers


lyTermParsers :: [Parser Term]
lyTermParsers = [relativeT, keyT, timeT, partialT, cadenzaT]

relativeT :: Parser Term
relativeT = Let . LetRelativePitch <$> (cmdsymbol "relative" *> lyPitch)

keyT :: Parser Term
keyT = Let . LetKey     <$> (cmdsymbol "key" *> keySig)

timeT :: Parser Term 
timeT = Let . LetMeter  <$> (cmdsymbol "time" *> timeSig)
     <?> "timeT"

partialT :: Parser Term 
partialT = Let . LetPartial  <$> (cmdsymbol "partial" *> lyDuration)

cadenzaT :: Parser Term
cadenzaT = cadenzaOnT <|> cadenzaOffT
  where
    cadenzaOnT  = Let (LetCadenza True)  <$ cmdsymbol "cadenzaOn" 
    cadenzaOffT = Let (LetCadenza False) <$ cmdsymbol "cadenzaOff"
--------------------------------------------------------------------------------
-- Parse the text for the water and holes so we can fill the holes

lyTextChunks :: TextChunkParser
lyTextChunks = collectWaterAcc metaOutput
  where 
    metaOutput = (,,) <$> lexeme (symbol "%{#")
                      <*> lexeme (symbol "output")
                      <*> manyTill anyChar (try $ string "#%}")  


cmdMode :: GenParser Char st Mode
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

    
    
timeSig :: GenParser Char st Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


keySig :: GenParser Char st Key
keySig  = (\(Pitch l a _) m -> Key (PitchLabel l a) m) 
    <$> lyPitch <*> cmdMode


lyPitch :: GenParser Char st Pitch
lyPitch = lexeme pch 
  where pch = Pitch <$> lyPitchLetter <*> lyAccidental <*> lyOctaveSpec

lyPitchLetter :: GenParser Char st PitchLetter
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
lyOctaveSpec :: GenParser Char st Int
lyOctaveSpec = option 4 (raised <|> lowered)
  where
    raised  = (3 +) <$> counting1 (char '\'')
    lowered = (3 -) <$> counting1 (char ',')
    
lyAccidental :: GenParser Char st Accidental
lyAccidental = option Nat (f <$> longestString accidentals) 
  where
    accidentals = ["isis", "eses", "is", "es"]
    f "isis" = DoubleSharp
    f "eses" = DoubleFlat
    f "is"   = Sharp
    f "es"   = Flat


lyDuration :: GenParser Char st Duration
lyDuration = 
    build <$> rootDuration <*> (counting $ symbol ".") 
                           <*> optparse (symbol "*" *> fractionalPart)
  where
    build d dc Nothing  = dotn dc d
    build d dc (Just r) = (dotn dc d) * r
    fractionalPart      = (\n d -> convRatio (n%d)) 
                              <$> int <*> option 1 (symbol "/" *> int)

rootDuration :: GenParser Char st Duration
rootDuration = choice [pBreve, pLonga, pNumericDuration]
  where
    pBreve            = breve <$ command "breve"   
    pLonga            = longa <$ command "longa"
    pNumericDuration  = (convRatio . (1%)) <$> int 
    

    
    
--------------------------------------------------------------------------------
-- utility parsers

-- | command - note using try is essential to consume the whole command
-- without it we may consume a blacklash of a different command and not be 
-- able to backtrack. 
command     :: String -> CharParser st String
command ss  = lexeme $ try $ string ('\\':ss)

