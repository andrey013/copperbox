{-# OPTIONS -Wall #-}


module Demo01 where

import Text.PrettyPrint.HughesPJ.PrettyExpr     -- package: hpj-pretty-expr
import Text.PrettyPrint.HughesPJ.PrettyExpr.Haskell

import qualified Text.PrettyPrint.HughesPJ.PrettyExpr.C99 as C
import qualified Text.PrettyPrint.HughesPJ.PrettyExpr.Ocaml as O

import Text.PrettyPrint.HughesPJ

data Expr = Int Int
          | Bool Bool
          | BinE BinOp Expr Expr
          | Neg Expr
   deriving (Show)

data BinOp = AddO | MinO | MulO | DivO
   deriving (Enum,Eq,Show)


buildExpr :: Expr -> DocE
buildExpr (Int i)        = Atom $ int i
buildExpr (Bool b)       = Atom $ text $ if b then "True" else "False"
buildExpr (BinE op a b)  = binaryDE op (buildExpr a) (buildExpr b)
buildExpr (Neg a)        = negateU (buildExpr a)



binaryDE :: BinOp -> DocE -> DocE -> DocE
binaryDE AddO = addB 
binaryDE MinO = subtractB
binaryDE MulO = multiplyB
binaryDE DivO = divB

ppTerm :: Expr -> Doc
ppTerm = unparse . buildExpr


infixl 6 +.
(+.) :: Expr -> Expr -> Expr 
(+.) = BinE AddO

infixl 6 -.
(-.) :: Expr -> Expr -> Expr 
(-.) = BinE MinO

infixl 7 *.
(*.) :: Expr -> Expr -> Expr 
(*.) = BinE MulO

infixl 7 /.
(/.) :: Expr -> Expr -> Expr 
(/.) = BinE DivO

expr01 :: Expr 
expr01 = Neg a
  where 
    a = (Int 3 +. Int 4) /. (Int 12 *. Int 2) 
   
     
demo01 :: IO ()
demo01 = print $ ppTerm expr01

expr02 :: Expr 
expr02 = Neg a
  where 
    a = Int 3 +. Int 4 /. (Int 12 *. Int 2) 

demo02 :: IO ()
demo02 = print $ ppTerm expr02
