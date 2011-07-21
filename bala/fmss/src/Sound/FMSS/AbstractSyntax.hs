{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.AbstractSyntax
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low-level abstract syntax
--
--------------------------------------------------------------------------------

module Sound.FMSS.AbstractSyntax
  (
    Instr(..)
  , VarId
  , Decl(..)
  , Expr(..)
  , Stmt(..)
  , CsValue(..)



  , mult
  , plus
  , minus
  , divide

  ) where


import Sound.FMSS.Utils.FormatCombinators
import Sound.FMSS.Utils.FormatExpr


data Instr = Instr 
      { instr_num           :: Int
      , instr_irate_decls   :: [Decl]
      , instr_body          :: [Stmt]
      } 
  deriving (Eq,Show)

type VarId = String

data Decl = CommentD String Decl
          | Decl VarId Expr
  deriving (Eq,Ord,Show)

data Expr = VarE   VarId
          | PField Int
          | Const  CsValue
          | UnOp   Rator Expr
          | BinOp  Rator Expr Expr
  deriving (Eq,Ord,Show)


data Stmt = CommentS String Stmt
          | Envelope VarId String Doc
          | Phasor   VarId Expr
          | Tablei   VarId Expr 
          | Out      VarId
  deriving (Eq,Show)

data CsValue = CsInt    Int
             | CsDouble Double
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------

mult :: Expr -> Expr -> Expr 
mult = BinOp mult_op

plus :: Expr -> Expr -> Expr
plus = BinOp plus_op

minus :: Expr -> Expr -> Expr
minus = BinOp minus_op

divide :: Expr -> Expr -> Expr
divide = BinOp divide_op


plus_op         :: Rator
plus_op         = infixL 6 "+"

minus_op        :: Rator
minus_op        = infixL 6 "-"

mult_op         :: Rator
mult_op         = infixL 7 "*"


divide_op       :: Rator
divide_op       = infixL 7 "/"

--------------------------------------------------------------------------------

instance Format Instr where
  format (Instr ix decls body) = 
     text "instr" <+> int ix            `vconcat`
     indent 2 (vcat $ map format decls) `vconcat`
     indent 2 (vcat $ map format body)  `vconcat`
     text "endin"

     

instance Format Decl where
  format (CommentD ss decl) = prependComment ss (format decl)

  format (Decl var expr)    = assignDoc var (format expr)

instance Format Stmt where
  format (CommentS ss stmt)        = prependComment ss (format stmt)

  format (Envelope  var opco body) = opcodeDoc var opco [body]

  format (Phasor var expr)         = opcodeDoc var "phasor" [format expr]

  format (Tablei var expr)         = opcodeDoc var "tablei" (format expr:rest)
    where
      rest = [ text "isinetbl", int 1, int 0, int 1 ]

  format (Out var)                 = 
    spaces 11 <+> padr 9 (text "out") <+> text var

instance Format Expr where
  format = unparse . buildExpr

buildExpr :: Expr -> DocExpr
buildExpr (VarE s)       = Atom $ text s
buildExpr (PField i)     = Atom $ char 'p' <> int i
buildExpr (Const val)    = Atom $ format val
buildExpr (UnOp op a)    = Unary op (buildExpr a)
buildExpr (BinOp op a b) = Binary (buildExpr a) op (buildExpr b)

instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d



prependComment :: String -> Doc -> Doc
prependComment ss = vconcat (char ';' <+> text ss)

assignDoc :: String -> Doc -> Doc
assignDoc name val = padr 11 (text name) <+> char '=' <+> val


-- |
opcodeDoc :: String -> String -> [Doc] -> Doc
opcodeDoc name opcode args = 
    padr 11 (text name) <+> padr 9 (text opcode) <+> commaSep args
