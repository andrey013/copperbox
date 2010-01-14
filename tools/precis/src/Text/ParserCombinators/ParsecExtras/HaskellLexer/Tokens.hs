{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ParsecExtras.HaskellLexer.Tokens
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
-- 
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ParsecExtras.HaskellLexer.Tokens
  (
    Token
  , Lexeme(..)

  , tokenPos
  , tokenClass
  , tokenSource

  ) where

import Text.ParserCombinators.Parsec.Pos

type Token = (SourcePos,Lexeme,String)


data Lexeme = Identifier 
            | Reserved   
            | IntLit
            | FloatLit            
            | CommentStart
            | Comment
  deriving (Eq,Show)

tokenPos  :: Token -> SourcePos
tokenPos (p,_,_) = p

tokenClass :: Token -> Lexeme
tokenClass (_,c,_) = c

tokenSource :: Token -> String
tokenSource (_,_,s) = s

