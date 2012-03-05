{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Parser
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser.
--
--------------------------------------------------------------------------------

module HMinCaml.Lexer 
   ( 

     MLParser

   , lexer

   , identifier
   , reservedOp
   , commaSep
   , int
   , double
   , reserved 
   , symbol
   , parens
   , dot

   ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P


type MLParser a = GenParser Char Int a

lexer :: P.TokenParser Int
lexer = P.makeTokenParser $ emptyDef
  { identStart        = letter <|> char '_'
  , identLetter       = alphaNum <|> oneOf "_'"
  , caseSensitive     = True
  , commentStart      = "(*"
  , commentEnd        = "*)"
  , opStart           = opLetter emptyDef
  , opLetter          = oneOf "+-=<>*/.;"  
  , reservedNames     = [ "if", "then", "else"
                        , "let", "in", "rec"
                        , "Array.create" 
                        , "true", "false"
                        ]
                        
  , reservedOpNames   = [ "+", "-", "=", "<>", "<=", ">=", "<", ">"
                        , "+.", "-.", "*.", "/."
                        , ";", "<-"
                        ]
  }

identifier      :: MLParser  String
identifier      = P.identifier lexer

reservedOp      :: String -> MLParser ()
reservedOp      = P.reservedOp lexer

commaSep        :: MLParser a -> MLParser [a]
commaSep        = P.commaSep lexer

int             :: MLParser Int
int             = P.integer lexer >>= return . fromIntegral

double          :: MLParser Double
double          = P.float lexer

reserved        :: String -> MLParser ()
reserved        = P.reserved lexer

symbol          :: String -> MLParser String
symbol          = P.symbol lexer

parens          :: MLParser a -> MLParser a
parens          = P.parens lexer


dot             :: MLParser String
dot             = P.dot lexer


    