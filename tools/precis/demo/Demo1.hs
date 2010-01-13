

module Demo where

import Precis.CabalData
import Precis.CabalExtract
import Precis.ParsecExtras


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Control.Applicative hiding ( (<|>), many )
import Control.Monad

import Data.Char

-- | Applicative cons.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

-- | Applicative T - reverse fmap.
(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap


test_file :: FilePath
test_file = "../samples/data-obscura.cabal"



demo1 = parseFromFile header test_file

demo2 = parseFromFile (do { h <- header; l <- library ; return l })
                      test_file



demo3 = parseFromFile (do { h  <- headerRegion (many1 anyChar)
                          ; xs <- libraryRegion (many1 anyChar) 
                          ; return xs
                          })

                       test_file


test1 = parse (symbolci "camelcase") "" "camelCase"



-- Lexical analysis
baseLex           :: P.TokenParser st
baseLex           = P.makeTokenParser emptyDef

symbol            :: String -> GenParser Char st String
symbol            = P.symbol baseLex

lexeme            :: GenParser Char st a -> GenParser Char st a
lexeme            = P.lexeme baseLex

symbolci = lexeme . stringci