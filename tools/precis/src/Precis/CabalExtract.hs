{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.CabalExtract
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- \'Fact extracting\' parser for Cabal files - i.e. it does not 
-- implement a full grammar for Cabal files, instead it only 
-- parses \'islands\' of interest.
--
--------------------------------------------------------------------------------


module Precis.CabalExtract
  (
  
    header_fields
  , library_fields
  , executable_fields

  , cabalField
  , header
  , library
  , executable

  ) where

import Precis.CabalData
import Precis.ParsecExtras

import Text.ParserCombinators.ParsecScreener

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 

import Control.Monad
import Data.Maybe


--------------------------------------------------------------------------------
-- fields extracted...

type FieldName = String

header_fields           :: [FieldName]
header_fields           = [ "name"
                          , "version"
                          , "description"
                          , "extra-source-files"
                          ]


library_fields          :: [FieldName]
library_fields          = [ "exposed-modules"
                          , "other-modules"
                          ]

executable_fields       :: [FieldName]
executable_fields       = [ "main-is"
                          , "other-modules"
                          ]


--------------------------------------------------------------------------------
-- delimiters

type Delimiter = ()

delimiter :: Parser a -> Parser Delimiter 
delimiter = liftM (const ())

library_tok         :: Parser String
library_tok         = assertColumn1 $ symbolci "library"

executable_tok      :: Parser String
executable_tok      = assertColumn1 $ symbolci "executable"


header_delim        :: Parser Delimiter 
header_delim        = delimiter (library_tok <|> executable_tok)

executable_delim    :: Parser Delimiter
executable_delim    = choice [ eof, delimiter library_tok, 
                                    delimiter executable_tok ]

library_delim       :: Parser Delimiter
library_delim       = choice [ eof, delimiter library_tok, 
                                    delimiter executable_tok ]

cabalField :: String -> Parser CabalField
cabalField name = withError "cabalField" $ 
    withIndent $ do { _   <- symbolci name
                    ; _   <- symbol ":" 
                    ; xs  <- many anyChar
                    ; return $ CabalField name xs
                    }
     
 
    
 
waterField :: Parser CabalField
waterField = withIndent $ do { name <- identifier 
                             ; _    <- symbol ":" 
                             ; body <- many anyChar
                             ; return $ CabalField name body
                             }
    

delimitedIW :: Parser a -> Parser b -> Parser Delimiter -> Parser [a]
delimitedIW island water delim = 
  liftM catMaybes $ manyUpto (islandWater island water) delim


header :: Parser [CabalField]
header = delimitedFields header_fields header_delim

library :: Parser [CabalField]
library = library_tok >> delimitedFields library_fields library_delim

executable :: Parser [CabalField]
executable = executable_tok >> identifier >> 
    delimitedFields executable_fields executable_delim


delimitedFields :: [FieldName] -> Parser Delimiter -> Parser [CabalField]
delimitedFields xs delim = delimitedIW islandField waterField delim
  where
    islandField :: Parser CabalField
    islandField = choice $ map cabalField xs



--------------------------------------------------------------------------------
-- Lexical analysis

cabalLexDef :: LanguageDef st
cabalLexDef = emptyDef 
    { commentLine   = "--"
    , identStart    = letter
    , identLetter   = letter <|> oneOf "-_"  
    }


cabalLexer          :: P.TokenParser st
cabalLexer          = P.makeTokenParser cabalLexDef

symbol              :: String -> CharParser st String
symbol              = P.symbol cabalLexer

lexeme              :: CharParser st a -> CharParser st a 
lexeme              = P.lexeme cabalLexer

symbolci            :: String -> CharParser st String
symbolci            = lexeme . try . stringci

identifier          :: CharParser st String
identifier          = P.identifier cabalLexer


