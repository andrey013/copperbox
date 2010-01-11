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
  , multilineField
  , cabalInfo

  ) where

import Precis.CabalData

import Text.ParserCombinators.ParsecScreener

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Control.Applicative hiding ( many )
import Control.Monad

instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap



onelineField :: String -> Parser String
onelineField name = symbol name >> symbol ":" >> withinLine (many anyChar)

multilineField :: String -> Parser String
multilineField name = symbol name >> symbol ":" >> withIndent (many anyChar)


cabalInfo :: Parser CabalInfo 
cabalInfo = CabalInfo <$> onelineField "name" <*> onelineField "version"
                      <*> (advanceLines 7 $ multilineField "description")

-- Lexical analysis
baseLex           :: P.TokenParser st
baseLex           = P.makeTokenParser emptyDef

symbol            :: String -> CharParser st String
symbol            = P.symbol baseLex