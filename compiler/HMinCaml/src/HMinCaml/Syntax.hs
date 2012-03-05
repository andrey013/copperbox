{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Syntax
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax.
--
--------------------------------------------------------------------------------

module HMinCaml.Syntax 
  ( 
    Expr(..)
  , Fundef(..)
  
  , ppExpr
  , ppFundef

  , pptExpr
  , pptFundef

  ) where

import HMinCaml.Id
import qualified HMinCaml.Type as T

import Text.PrettyPrint.HughesPJ.PrettyExpr.Ocaml

import Text.PrettyPrint.HughesPJ

import Data.List ( intersperse )

data Expr = Unit
          | Bool Bool
          | Int Int
          | Float Double
          | Not Expr
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | FNeg Expr
          | FAdd Expr Expr
          | FSub Expr Expr
          | FMul Expr Expr
          | FDiv Expr Expr
          | Eq Expr Expr
          | LE Expr Expr
          | If Expr Expr Expr
          | Let (IdS,T.Type) Expr Expr
          | Var IdS
          | LetRec Fundef Expr
          | App Expr [Expr]
          | Tuple [Expr]
          | LetTuple [(IdS,T.Type)] Expr Expr
          | Array Expr Expr
          | Get Expr Expr 
          | Put Expr Expr Expr
  deriving (Eq, Show)


data Fundef = Fundef 
    { fun_head       :: (IdS, T.Type)
    , fun_args       :: [(IdS, T.Type)]
    , fun_body       :: Expr
    } 
  deriving (Eq, Show)


ppExpr :: Expr -> Doc
ppExpr = unparse . exprDoc

exprDoc :: Expr -> DocE
exprDoc Unit                  = literal "()"
exprDoc (Bool b)              = if b then literal "true" else literal "false"
exprDoc (Int i)               = Atom $ int i 
exprDoc (Float d)             = Atom $ double d
exprDoc (Not e)               = notU $ exprDoc e
exprDoc (Neg e)               = inegateU $ exprDoc e
exprDoc (Add e1 e2)           = iaddB (exprDoc e1) (exprDoc e2)
exprDoc (Sub e1 e2)           = isubtractB (exprDoc e1) (exprDoc e2)
exprDoc (FNeg e)              = fnegateU $ exprDoc e
exprDoc (FAdd e1 e2)          = faddB (exprDoc e1) (exprDoc e2)
exprDoc (FSub e1 e2)          = fsubtractB (exprDoc e1) (exprDoc e2)
exprDoc (FMul e1 e2)          = fmultiplyB (exprDoc e1) (exprDoc e2)
exprDoc (FDiv e1 e2)          = fdivideB (exprDoc e1) (exprDoc e2)
exprDoc (Eq e1 e2)            = structuralEqB (exprDoc e1) (exprDoc e2)
exprDoc (LE e1 e2)            = lessThanEqB  (exprDoc e1) (exprDoc e2)

-- if actually has precedence 2....
exprDoc (If e1 e2 e3)         = 
    Atom $ text "if" <+> ppExpr e1 <+> text "then" <+> ppExpr e2 
                                   <+> text "else" $+$ ppExpr e3

exprDoc (Let xt e1 e2)       = 
    Atom $ text "let" <+> (ppBind xt e1) <+> text "in" <+> ppExpr e2

exprDoc (Var x)              = literal x
exprDoc (LetRec fd e)        = 
    Atom $ ppFundef fd <+> text "in" <+> ppExpr e


exprDoc (App e es)           = funAppB (exprDoc e) (map exprDoc es)

exprDoc (Tuple es)           = 
    Atom $ parens $ hcat $ intersperse comma $ map ppExpr es

-- precedence 0 ...
exprDoc (LetTuple xts e1 e2) = 
    Atom $ text "let" <+> ppTupleVars xts <+> char '=' <+> ppExpr e1
                      <+> text "in" <+> ppExpr e2

exprDoc (Array e1 e2)        = 
    funAppB (literal "Array.create") [exprDoc e1, exprDoc e2]


exprDoc (Get e1 e2)          = 
    Atom $ ppExpr e1 <> char '.' <> parens (ppExpr e2)

exprDoc (Put e1 e2 e3)       = 
    Atom $ ppExpr e1 <> char '.' <> parens (ppExpr e2) 
                     <+> text "<-" <+> ppExpr e3



ppFundef :: Fundef -> Doc
ppFundef (Fundef hd args e) = 
    text "let rec" <+> text (fst hd) <+> hsep (map (text .fst) args) <+> char '=' 
      $+$ nest 2 (ppExpr e)

ppBind :: (IdS, T.Type) -> Expr -> Doc
ppBind (x,_) e = text x <+> char '=' <+> ppExpr e

ppTupleVars :: [(IdS, T.Type)] -> Doc
ppTupleVars = parens . hcat . intersperse comma . map (text . fst)

--------------------------------------------------------------------------------
-- ppt - pretty print with type info


pptExpr :: Expr -> Doc
pptExpr = unparse . exptDoc

exptDoc :: Expr -> DocE
exptDoc (Let xt e1 e2)       = 
    Atom $ text "let" <+> (pptBind xt e1) <+> text "in" <+> pptExpr e2

exptDoc (LetRec fd e)        = 
    Atom $ pptFundef fd <+> text "in" <+> pptExpr e

exptDoc (LetTuple xts e1 e2) = 
    Atom $ text "let" <+> ppTupleVars xts 
                      <+> char ':' <+> pptTupleType xts
                      <+> char '=' <+> ppExpr e1
                      <+> text "in" <+> ppExpr e2

exptDoc e                    = exprDoc e


pptFundef :: Fundef -> Doc
pptFundef fd@(Fundef hd args _) = funtype $+$ ppFundef fd
  where
    funtype = text "val" <+> text (fst hd) 
                         <+> char ':' <+> T.ppType (T.Fun (map snd args) (snd hd))



pptBind :: (IdS, T.Type) -> Expr -> Doc
pptBind (x,t) e = 
    text x <+> char ':' <+> T.ppType t <+> char '=' <+> ppExpr e

pptTupleType :: [(IdS, T.Type)] -> Doc
pptTupleType = T.ppType . T.Tuple . map snd

