{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Lexer
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Parser
--

module HMinCaml.Lexer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer :: P.TokenParser ()
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

identifier      :: CharParser () String
identifier      = P.identifier lexer

reservedOp      :: String -> CharParser () ()
reservedOp      = P.reservedOp lexer

commaSep        :: CharParser () a -> CharParser () [a]
commaSep        = P.commaSep lexer

int             :: CharParser () Int
int             = P.integer lexer >>= return . fromIntegral

float           :: CharParser () Float
float           = P.float lexer >>= return . fromRational . toRational

reserved       :: String -> CharParser () ()
reserved        = P.reserved lexer

symbol          :: String -> CharParser () String
symbol          = P.symbol lexer




parens          :: CharParser () a -> CharParser () a
parens          = P.parens lexer


dot             :: CharParser () String
dot             = P.dot lexer


    