
--------------------------------------------------------------------------------
-- |
-- Module      :  ParserBase
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

module ParserBase where


import ExtractionDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad
import Data.List (sortBy)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)


-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap



data Nested = NestStart | NestEnd
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Meta-comments

metamark = MetaMark <$> sourcePosition <*> mcommand 

mcommand = choice [moutput]

moutput :: Parser MetaDirective
moutput = MetaOutput
    <$> (keyword "output" *> identifier) <*> (colon *> identifier)


--------------------------------------------------------------------------------
-- Utility functions

manyWaterIsland :: GenParser Char st a -> GenParser Char st [a]
manyWaterIsland p = many $ try (water p) 


-- Note - the supplied parser can't match against eof! 
water :: GenParser Char st a -> GenParser Char st a
water p = do
    a <- optparse (eitherparse (eof <?> "") p)    
    case a of
      Just (Left _) -> fail "water - eof reached - parse failed"
      Just (Right ans) -> return ans
      Nothing -> anyChar >> water p 
  <?> "water"      

waterMaybe :: GenParser Char st a -> GenParser Char st (Maybe a)
waterMaybe p = do
    a <- optparse (eitherparse (eof <?> "") p)    
    case a of
      Just (Left _) -> return Nothing
      Just (Right ans) -> return (Just ans)
      Nothing -> anyChar >> waterMaybe p 
  <?> "waterMaybe" 
  
      
sourcePosition :: GenParser Char st SrcPos
sourcePosition = makeSrcPos <$> getPosition


makeSrcPos pos = SrcPos {
    _src_line     = sourceLine pos,
    _src_column   = sourceColumn pos,
    _src_file     = sourceName pos
  } 

keyword = symbol

chooseString :: [String] -> GenParser Char st String  
chooseString = choice . map string

-- | Match the longest string.
longestString :: [String] -> GenParser Char st String
longestString = choice . map (try . string) . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)
  
    
charDrop :: Char -> GenParser Char st ()
charDrop a = char a >> return () 

-- | Return the count of the number of parses, rather than a list of elements.
-- (Note the @count@ combinator in Parsec works differently, it will parse a 
-- element n times).
counting :: GenParser Char st a -> GenParser Char st Int
counting  p = length <$> many p

-- | Version of @counting@ that must succeed at least once.
counting1 :: GenParser Char st a -> GenParser Char st Int
counting1 p = length <$> many1 p



-- | An alternative to Parsec's @option@ parser. Whereas option returns a 
-- default value if the parse fails, optparse wraps success and failure in
-- a Maybe.
optparse :: GenParser Char st a -> GenParser Char st (Maybe a)
optparse p = option Nothing (try $ Just <$> p)

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