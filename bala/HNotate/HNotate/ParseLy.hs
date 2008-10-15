
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


lyExprView_TwoPass :: FilePath -> IO (Either ParseError [Expr])
lyExprView_TwoPass = twoPass preprocessLy parseLyExprs

--------------------------------------------------------------------------------
-- Preprocess

preprocessLy :: FilePath -> IO (Either ParseError String)
preprocessLy path = either (return . Left) (return . Right . hyloLy) 
                          =<< (parseFromFile lyExtract path)
                          
lyExtract :: Parser (Seq String)
lyExtract = waterAcc $ choice 
    [ metaComment, lyComment, leftBrace, rightBrace, 
      -- commands
      key, time, relative, cadenzaOn, cadenzaOff
      
    ]


-- This might be too simplistic...
-- - get rid of nested blocks {}
hyloLy :: Seq String -> String
hyloLy se = hylo step addsep "" (0,se)
  where
    step (i,se)           = phi (i, viewl se)
    
    -- exit the unfold
    phi (_,EmptyL)        = Nothing
    
   
    -- 
    phi (i, "{" :< se)    = case viewl se of
                                  -- "{}" - get rid of both
                              "}" :< sa -> Just ("", (i,sa))
                              
                                  -- "{{" - drop the first, incr the count 
                              "{" :< _  -> Just ("", (i+1,se))
                              
                                  -- "{ something useful ..."  
                              _         -> Just ("{", (i,se))
    
    -- one way down - 
    phi (i, "}" :< se)    | i > 0       = Just ("",  (i-1,se)) 
                          | otherwise   = Just ("}", (0,se))
                         
    -- normal case - produce value and go next
    phi (i, e :< se)      = Just  (e, (i,se)) 

    addsep xs ys = xs ++ (' ':ys)

-- first part needs try so we don't consume the "%{" of a normal comment
metaComment :: Parser (TokenF String)
metaComment = token1 fn <$> 
    ((try $ string "%{#") *> manyTill anyChar (try $ string "#%}"))
  where
    fn s = "%{#" ++ s ++ "#%}"
                
lyComment :: Parser (TokenF String)
lyComment = dropToken <$
    (symbol "%{" *> manyTill anyChar (try $ symbol "%}")) 

leftBrace :: Parser (TokenF String)
leftBrace = token1 id <$> symbol "{"

rightBrace :: Parser (TokenF String)
rightBrace = token1 id <$> symbol "}"

-- commands

key :: Parser (TokenF String)
key = token3 id id id <$> cmdsymbol "key" <*> nonwhite <*> cmdany


time :: Parser (TokenF String)
time = token2 id id <$> cmdsymbol "time" <*> nonwhite

relative :: Parser (TokenF String)
relative = token2 id id <$> cmdsymbol "relative" <*> nonwhite

cadenzaOn :: Parser (TokenF String)
cadenzaOn = token1 id <$> cmdsymbol "cadenza"

cadenzaOff :: Parser (TokenF String)
cadenzaOff = token1 id <$> cmdsymbol "cadenzaOff"


cmdany :: Parser String
cmdany = (:) <$> char '\\' <*> nonwhite

--------------------------------------------------------------------------------
-- Parse

parseLyExprs :: Parser [Expr]
parseLyExprs = topLevelExprs lyTermParsers


lyTermParsers :: [Parser Term]
lyTermParsers = [relativeT, keyT, timeT]

relativeT :: Parser Term
relativeT = Let . LetRelativePitch <$> (cmdsymbol "relative" *> lyPitch)

keyT :: Parser Term
keyT = Let . LetKey     <$> (cmdsymbol "key" *> keySig)

timeT :: Parser Term 
timeT = Let . LetMeter  <$> (cmdsymbol "time" *> timeSig)
     <?> "timeT"

--------------------------------------------------------------------------------
-- Parse the text for the water and holes so we can fill the holes

lyTextChunks :: Parser (Seq TextChunk)
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
keySig  = (\(Pitch l a _) m -> Key l a m) 
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

