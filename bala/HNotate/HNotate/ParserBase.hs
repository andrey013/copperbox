
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParserBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParserBase - utilty parsers and meta-comment parsing.
--
--------------------------------------------------------------------------------

module HNotate.ParserBase where

import HNotate.CommonUtils hiding (build)
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.TemplateDatatypes
import HNotate.PPInstances

import Control.Applicative hiding (many, optional, (<|>), empty )
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.List (sortBy, intersperse, filter)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (length, reverse)
import Prelude hiding (null)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)


-- ExprParser returns the 'expressions of interest' - the parts
-- of the input file that we know how to interpret. 
type ExprParser = FilePath -> NotateT IO (Either ParseError [Expr])

-- TextChunkParser returns a 'text view' of the input file - 
-- source to be preserved plus locations of holes to be plugged.
type TextChunkParser = Parser (Seq TextChunk)

  
-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap


metaOutput :: Parser MetaOutput
metaOutput = MetaOutput
    <$> (directive "output" *> optionMaybe outputCommand) <*> identifier 
  where 
    outputCommand = cmdrelative <|> cmddefault
    cmdrelative   = OutputRelative <$ command "relative"   
    cmddefault    = OutputDefault  <$ command "default"
    
    
metaMeterPattern :: Parser MetaBinding
metaMeterPattern = MetaMeterPattern
    <$> (directive "meter_pattern" *> meterPattern)


metaPartial :: Parser MetaBinding
metaPartial = MetaPartial
    <$> (directive "partial" *> eqnDuration)


meterPattern :: Parser MeterPattern
meterPattern = (,) <$> sepBy1 int plus <*> (slash *> simpleDuration)
  where 
    plus  = symbol "+"  
    slash = symbol "/"  

eqnDuration :: Parser Duration
eqnDuration = (\n d mul -> (n%d) * (mul%1)) 
  <$> integer <*> (symbol "%" *> integer) 
              <*> option 1 (symbol "*" *> integer)

simpleDuration :: Parser Duration
simpleDuration = (convRatio . (1%)) <$> int 
    
-- | command - note using try is essential to consume the whole command.
-- Without it we may consume a blacklash of a different command and not be 
-- able to backtrack. 
command     :: String -> CharParser st String
command s   = lexeme $ try $ string ('\\':s)   

directive :: String -> Parser String
directive s = try (symbol s) <* lexeme colon


--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 


-- Collect water to the left of the output directive.
-- Because both views are interpreted topdown, left-to-right
-- we don't need to store what the output directive says
-- (actually we don't really need is src position either).
-- The last element of sequence should always be (water,Nothing),
-- and the 'inits' should be (water, Just pos). 
-- Water may sometimes be "" (empty).

collectWaterAcc :: GenParser Char st a -> GenParser Char st (Seq TextChunk)
collectWaterAcc p = step empty "" <?> "collectWaterAcc"
  where 
    step se retaw = do
      a <- optionMaybe $ eitherparse (eof <?> "") (withPos p)    
      case a of
          -- Eof
        Just (Left _)         -> return $ se |> (reverse retaw, Nothing)
        
          -- p matched
        Just (Right (pos,_))  -> step (se |> (reverse retaw, Just pos)) ""
                
          -- more water
        Nothing               -> anyChar >>= \c -> step se (c:retaw) 

withPos :: GenParser Char st a -> GenParser Char st (SrcPos,a)        
withPos p = (,) <$> sourcePosition <*> p   


    
--------------------------------------------------------------------------------
-- Utility functions

-- pushBack - parse input, put the consumed input back on the input stream,
-- return the answer. 
pushBack :: GenParser Char st a -> GenParser Char st a
pushBack p = do
  s   <- getInput 
  ans <- p
  setInput s
  return ans  
  
stringTill :: GenParser Char st a -> GenParser Char st String
stringTill p = manyTill anyChar (pushBack p)    
              
sourcePosition :: GenParser Char st SrcPos
sourcePosition = makeSrcPos <$> getPosition


makeSrcPos pos = SrcPos {
    _src_line     = sourceLine pos,
    _src_column   = sourceColumn pos,
    _src_file     = sourceName pos
  } 


chooseString :: [String] -> GenParser Char st String  
chooseString = choice . map string

-- | Match the longest string.
longestString :: [String] -> GenParser Char st String
longestString = choice . map (try . string) . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)
  

-- parse a character but return unit    
charDrop :: Char -> GenParser Char st ()
charDrop a = char a >> return () 

nonwhite :: GenParser Char st String
nonwhite = lexeme (many1 nonwhiteChar)

nonwhiteChar :: GenParser Char st Char
nonwhiteChar = satisfy $ not . isSpace 

-- | Return the count of the number of parses, rather than a list of elements.
-- (Note the @count@ combinator in Parsec works differently, it will parse a 
-- element n times).
counting :: GenParser Char st a -> GenParser Char st Int
counting  p = length <$> many p

-- | Version of @counting@ that must succeed at least once.
counting1 :: GenParser Char st a -> GenParser Char st Int
counting1 p = length <$> many1 p



-- | Wrap Parser's alterative (\<|\>) combinator with the Either type to 
-- get different types for the left and right parse.
eitherparse :: GenParser Char st a 
            -> GenParser Char st b 
            -> GenParser Char st (Either a b)
eitherparse p p' = (Left <$> p) <|> (Right <$> p')


--------------------------------------------------------------------------------
-- Lexical analysis

baseLex           = P.makeTokenParser emptyDef

-- | @lexeme@ from ParsecChar.
lexeme            :: CharParser st a -> CharParser st a
lexeme            = P.lexeme baseLex

symbol            :: String -> CharParser st String
symbol            = P.symbol baseLex

identifier        :: CharParser st String
identifier        = P.identifier baseLex

integer           :: CharParser st Integer
integer           = P.integer baseLex

int               :: CharParser st Int
int               = fromIntegral <$> integer 

colon             :: CharParser st String
colon             = P.colon baseLex

braces            :: CharParser st a -> CharParser st a
braces            = P.braces baseLex

commaSep          :: CharParser st a -> CharParser st [a]
commaSep          = P.commaSep baseLex

whiteSpace        :: CharParser st ()
whiteSpace        = P.whiteSpace baseLex

