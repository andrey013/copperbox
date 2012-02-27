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


module Language.GLSL.TokStream 
  ( 
    tokensGlsl
  ) where

import Language.GLSL.Lex
import Language.GLSL.ParseMonad
import Language.GLSL.Syntax
import Language.GLSL.Token


import Control.Monad.Identity

}



%name glslTokens token_stream

%lexer { glslLex } { L _ Tk_EOF }
%monad { ParseM } { >>= } { return }
%error { parseError }


%tokentype { Lexeme }

%token 
  TOKEN               { L _ _ }


%%


token_stream :: { [Lexeme] }
  : token_list                 { toListH $1 }
  
token_list :: { H Lexeme }
  : token                      { wrapH $1 }
  | token_list token           { $1 `snocH` $2 }  

token :: { Lexeme }
  : TOKEN                      { $1 }


{


-- Hughes List

type H a = [a] -> [a]

wrapH :: a -> H a
wrapH = (:)

snocH :: H a -> a -> H a
snocH f a = f . (a:)

emptyH :: H a
emptyH = id

toListH :: H a -> [a]
toListH f = f $ []


tokensGlsl :: FilePath -> String -> Either String [Lexeme]
tokensGlsl path contents = 
    case runIdentity (runParseT glslTokens path contents) of
      Left (ParseErr err) -> Left err
      Right a             -> Right a
    
    


}  
