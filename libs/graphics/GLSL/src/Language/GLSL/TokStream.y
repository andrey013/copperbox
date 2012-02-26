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

snocDeclr :: Declrs -> DeclrElement -> Declrs
snocDeclr (Declr ty xs)         x = Declr ty (xs++[x])
snocDeclr (InvariantDeclr s xs) x = InvariantDeclr s (xs++[x])

tokensGlsl :: FilePath -> String -> Either String [Lexeme]
tokensGlsl path contents = 
    case runIdentity (runParseT glslTokens path contents) of
      Left (ParseErr err) -> Left err
      Right a             -> Right a
    
    

unwrapExpr :: Stmt -> Expr
unwrapExpr (ExprStmt (Just e))  = e
unwrapExpr _                    = error $ "fail"

mkIfStmt :: Expr -> (Stmt, Maybe Stmt) -> Stmt
mkIfStmt condE (thenS,opt_elseS) = IfStmt condE thenS opt_elseS

mkFor :: Expr -> (Expr, Maybe Expr) -> Stmt -> Stmt
mkFor initE (condE,opt_loopE) bodyS = For initE condE opt_loopE bodyS

constructorIdent :: TypeSpec -> Ident
constructorIdent ts = case ts of
    ScalarType ty -> fn ty
    ArrayType ty _ -> fn ty
  where     
    fn Vec2             = "vec2"
    fn Vec3             = "vec3"
    fn Vec4             = "vec4"
    fn BVec2            = "bvec2"
    fn BVec3            = "bvec3"
    fn BVec4            = "bvec4"
    fn IVec2            = "ivec2"
    fn IVec3            = "ivec3"
    fn IVec4            = "ivec4"
    fn Mat2             = "mat2"
    fn Mat3             = "mat3"
    fn Mat4             = "mat4"
    fn Mat2x2           = "mat2x2"
    fn Mat2x3           = "mat2x3"
    fn Mat2x4           = "mat2x4"
    fn Mat3x2           = "mat3x2"
    fn Mat3x3           = "mat3x3"
    fn Mat3x4           = "mat3x4"
    fn Mat4x2           = "mat4x2"
    fn Mat4x3           = "mat4x3"
    fn Mat4x4           = "mat4x4"
    fn z                = error $ "constructorIdent on " ++ show z

 


}  
