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
    onelineField 
  , field
  , cabalField
  , header
  , cabalInfo

  ) where

import Precis.CabalData
import Precis.ParsecExtras

import Text.ParserCombinators.ParsecScreener

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 

import Control.Applicative hiding ( many, (<|>) )
import Control.Monad
import Data.Maybe


onelineField :: String -> Parser String
onelineField name = withinLine $ 
    symbolci name >> symbol ":" >> many anyChar


field :: String -> Parser String
field name = withIndent $ symbolci name >> symbol ":" >> many anyChar

cabalField :: String -> Parser CabalField
cabalField name = withError ("cabal field - unrecognized " ++ name) $ 
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
    

header :: [String] -> Parser [CabalField]
header xs = catMaybes <$> manyTill (islandWater fields waterField) (try $ symbol "library")
  where
    fields :: Parser CabalField
    fields = choice $ map cabalField xs



cabalInfo :: Parser CabalInfo 
cabalInfo = CabalInfo <$> onelineField "name" <*> onelineField "version"
                      <*> (advanceLines 7 $ liftM Just $ field "description")

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


