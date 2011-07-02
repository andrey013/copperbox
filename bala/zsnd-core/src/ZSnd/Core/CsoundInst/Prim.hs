{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst.Prim
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Dynmically typed internals of instrument defs.
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst.Prim
  (

  -- * Data types
    PrimInst(..)
  , Stmt(..)
  , Expr(..)
  , CsValue(..)
  , TagVar(..)
  , DataRate(..)

  -- * Re-exports for expressions
  , Rator
  , prefix
  , postfix
  , infixL
  , infixR

  , ppPrimInst

  ) where


import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.FormatExpr


--------------------------------------------------------------------------------
-- Inst


data PrimInst = PrimInst 
      { inst_num          :: Int
      , inst_table_start  :: Int
      , inst_body         :: [Stmt]
      }
  deriving (Eq,Ord,Show)


--
-- Note - cannot use labels and blocks cf. the Legato instruments
-- chapter in the Csound book. 
--



-- | For convenience Outs are made a constructor, they could be a 
-- nullary opcode.
--
-- Update - maybe Outs is not convenient for translating.... 
--
data Stmt = Vardef      TagVar Expr
          | Opcode      [TagVar] String [Expr]
          | Outs        [Expr]
  deriving (Eq,Ord,Show)



-- | Dynamically typed expr
-- 
data Expr = VarE      TagVar
          | PField    Int
          | TableRef  Int
          | Literal   CsValue
          | ZeroOp    String
          | UnOp      Rator Expr
          | BinOp     Rator Expr Expr
          | Funcall   String Expr
  deriving (Eq,Ord,Show)


data CsValue = CsInt    Int
             | CsDouble Double
             | CsString String          -- for file names
  deriving (Eq,Ord,Show)


-- | Type tagged variable
-- 
-- The type is tagged in the constructor (not at the type level)
-- this is to allow DExpr to account for all types.
--
data TagVar = GblVar DataRate Int
            | LocVar DataRate Int
  deriving (Eq,Ord,Show)



-- | CSB page22.
--
data DataRate = I | K | A 
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Format instances


ppPrimInst :: PrimInst -> Doc
ppPrimInst (PrimInst n i ds) = 
      (text "instr" <+> int n) `vconcat` body `vconcat` text "endin"
    where
      body = vcat $ map (ppStmt i) ds
                           

ppStmt :: Int -> Stmt -> Doc
ppStmt ix (Vardef v1 val)    = 
    padr 7 (format v1) <+> char '=' <+> ppExpr ix val

ppStmt ix (Opcode vs name xs) = 
    padr 7 (dlist $ map format vs) <+> padr 11 (text name) 
                                   <+> dlist (map (ppExpr ix) xs)
      where
        dlist = punctuate (text ", ")


ppStmt ix (Outs [x])           = 
    indent 8 (padr 11 (text "out") <+> ppExpr ix x)
  
ppStmt ix (Outs xs)            = 
    indent 8 (padr 11 (text "outs") <+> punctuate (text ", ") 
                                                  (map (ppExpr ix) xs))

                      

ppExpr :: Int -> Expr -> Doc
ppExpr i = unparse . buildExpr i

buildExpr :: Int -> Expr -> DocExpr
buildExpr _  (VarE s)       = Atom $ format s
buildExpr _  (PField i)     = Atom $ char 'p' <> int i 
buildExpr ix (TableRef i)   = Atom $ int $ i + ix 
buildExpr _  (Literal val)  = Atom $ format val
buildExpr _  (ZeroOp ss)    = Atom $ text ss
buildExpr ix (UnOp op a)    = Unary op (buildExpr ix a)
buildExpr ix (BinOp op a b) = Binary (buildExpr ix a) op (buildExpr ix b)
buildExpr ix (Funcall ss a) = Atom $ text ss <> parens (ppExpr ix a)



instance Format TagVar where
  format (GblVar rt i)  = char 'g' <> format rt <> int i
  format (LocVar rt i)  = format rt <> int i


instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d
  format (CsString s) = dquotes $ text s



instance Format DataRate where
  format I = char 'i'
  format K = char 'k'
  format A = char 'a'
