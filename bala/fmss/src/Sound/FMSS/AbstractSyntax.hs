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

  , SymDouble
  , idur

  ) where


import Sound.FMSS.Utils.FormatCombinators
import Sound.FMSS.Utils.FormatExpr

import Data.String


data Instr = Instr 
      { instr_number        :: Int
      , instr_irate_decls   :: [Decl]
      , instr_body          :: [Stmt]
      } 
  deriving (Eq,Show)

type VarId = String


-- | Calls to invoke envelopes (@linseg@, @expseg@, etc.) are 
-- considered declarations, even though in Csound they are 
-- /opcode statements/ so are syntactically the same as  @phasor@ 
-- or @tablei@. This is for convenience within FMSS.
--
--
data Decl = CommentD String Decl
          | Decl     VarId Expr
          | Envelope VarId String [SymDouble]
  deriving (Eq,Ord,Show)


data Expr = VarE    VarId
          | PField  Int
          | Const   CsValue
          | UnOp    Rator Expr
          | BinOp   Rator Expr Expr
          | Funcall String Expr
  deriving (Eq,Ord,Show)


data Stmt = CommentS String Stmt
          | Phasor   VarId Expr
          | Tablei   VarId Expr 
          | Assign   VarId Expr
          | Out      Expr
  deriving (Eq,Show)

data CsValue = CsInt    Int
             | CsDouble Double
  deriving (Eq,Ord,Show)



-- | Symbolic number type for describing envelopes.
-- 
data SymDouble = SDouble Double
               | IDur
               | SUnOp    Rator SymDouble
               | SBinOp   Rator SymDouble SymDouble
               | SFuncall String SymDouble
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------



plus_op         :: Rator
plus_op         = infixL 6 "+"

minus_op        :: Rator
minus_op        = infixL 6 "-"

mult_op         :: Rator
mult_op         = infixL 7 "*"


divide_op       :: Rator
divide_op       = infixL 7 "/"

unary_negate    :: Rator
unary_negate    = prefix 9 "-"


instance Num Expr where
  (+)     = BinOp plus_op
  (-)     = BinOp minus_op
  (*)     = BinOp mult_op
  abs     = Funcall "abs"
  negate  = UnOp unary_negate
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Const $ CsInt $ fromInteger i


instance Fractional Expr where
  (/)     = BinOp divide_op
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = Const $ CsDouble (fromRational d)

instance IsString Expr where
  fromString ss = VarE ss

instance Num SymDouble where
  (+)     = SBinOp plus_op
  (-)     = SBinOp minus_op
  (*)     = SBinOp mult_op
  abs     = SFuncall "abs"
  negate  = SUnOp unary_negate
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = SDouble $ fromInteger i


instance Fractional SymDouble where
  (/)     = SBinOp divide_op
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = SDouble $ fromRational d

-- | @idur@ is a symbolic constant in envelope expressions.
--
idur :: SymDouble
idur = IDur

--------------------------------------------------------------------------------

instance Format Instr where
  format (Instr ix decls body) = 
     text "instr" <+> int ix            `vconcat`
     indent 2 (vcat $ map format decls) `vconcat`
     indent 2 (vcat $ map format body)  `vconcat`
     text "endin"

     

instance Format Decl where
  format (CommentD ss decl)        = prependComment ss (format decl)

  format (Decl var expr)           = assignDoc var (format expr)

  format (Envelope  var opco body) = opcodeDoc var opco (map format body)


instance Format Stmt where
  format (CommentS ss stmt)        = prependComment ss (format stmt)

  format (Phasor var expr)         = opcodeDoc var "phasor" [format expr]

  format (Tablei var expr)         = opcodeDoc var "tablei" (format expr:rest)
    where
      rest = [ text "isinetbl", int 1, int 0, int 1 ]

  format (Assign var expr)         = assignDoc var (format expr)

  format (Out expr)                = 
    spaces 11 <+> padr 9 (text "out") <+> format expr


instance Format Expr where
  format = unparse . buildExpr

buildExpr :: Expr -> DocExpr
buildExpr (VarE s)       = Atom $ text s
buildExpr (PField i)     = Atom $ char 'p' <> int i
buildExpr (Const val)    = Atom $ format val
buildExpr (UnOp op a)    = Unary op (buildExpr a)
buildExpr (BinOp op a b) = Binary (buildExpr a) op (buildExpr b)
buildExpr (Funcall ss a) = Atom $ text ss <> parens (format a)

instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d



instance Format SymDouble where
  format = unparse . buildSymExpr

buildSymExpr :: SymDouble -> DocExpr
buildSymExpr IDur            = Atom $ text "idur"
buildSymExpr (SDouble d)     = Atom $ dtrunc d
buildSymExpr (SUnOp op a)    = Unary op (buildSymExpr a)
buildSymExpr (SBinOp op a b) = Binary (buildSymExpr a) op (buildSymExpr b)
buildSymExpr (SFuncall ss a) = Atom $ text ss <> parens (format a)



prependComment :: String -> Doc -> Doc
prependComment ss doc = vcat [text "", char ';' <+> text ss, doc]

assignDoc :: String -> Doc -> Doc
assignDoc name val = padr 11 (text name) <+> char '=' <+> val


-- |
opcodeDoc :: String -> String -> [Doc] -> Doc
opcodeDoc name opcode args = 
    padr 11 (text name) <+> padr 9 (text opcode) <+> commaSep args
