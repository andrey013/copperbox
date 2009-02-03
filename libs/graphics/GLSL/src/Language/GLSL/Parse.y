--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Parse
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Lexer
-- 
--------------------------------------------------------------------------------

{
{-# OPTIONS -Wall #-}

module Language.GLSL.Parse where

import Language.GLSL.Lex
import Language.GLSL.ParseMonad
import Language.GLSL.Tokens


import Control.Monad.Identity



parseGlsl :: FilePath -> String -> Integer
parseGlsl path contents  = 
  case runIdentity (runParseT glslParser path contents) of
    Left err -> error "ERRR"
    Right a -> a 

}

%name glslParser

%lexer { glslLex } { L _ Tk_EOF }
%monad { ParseM } { >>= } { return }
%error { parseError }


%tokentype { Lexeme }

%token 
  ':'   { L _ Tk_Period }
  INT   { L _ (Tk_Integer $$) }
  


  
%%

int :: { Integer }
  : INT { $1 }
  
{

-- some haskell



}  