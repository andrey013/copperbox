{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.PrettyPrint
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty printer for the Primitive language.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.PrettyPrint
  (
    
    prettyPrint

  , ppInstDef

  ) where

import Orchsyn.Language.Expr
import Orchsyn.Language.PrimAst
import Orchsyn.Utils.PrettyExtras


import Text.PrettyPrint.HughesPJ




prettyPrint :: OrchDef -> Doc
prettyPrint = ppOrchDef


ppOrchDef :: OrchDef -> Doc
ppOrchDef (OrchDef []    insts) = vconcatSpace $ map ppInstDef insts
ppOrchDef (OrchDef gvars insts) = 
    vconcatSpace [ vconcatSpace $ map ppGlobal gvars
                 , vconcatSpace $ map ppInstDef insts
                 ]



ppGlobal :: Global -> Doc
ppGlobal (AssignG s e)     = 
    indent $ padStringR 9 (varname s) <+> char '=' <+> format e

ppGlobal (OpcodeG s op es) = 
    indent $ padStringR 9 (varname s) <+> padStringR 9 op <+> arglist (map format es)

ppInstDef :: InstDef -> Doc
ppInstDef (InstDef name args vars stmts) = 
    vconcatSpace [ instr_start
                 , vconcatSpace [ arg_block
                                , var_block
                                , stmt_block
                                ]
                 , instr_end 
                 ]
      where
        instr_start = text "instr" <+> int name
        arg_block   = vconcat $ map ppArgDef args
        var_block   = vconcat $ map ppVarDef vars
        stmt_block  = vconcat $ map ppPrimStmt stmts
        instr_end   = text "endin"   


ppArgDef :: ArgDef -> Doc
ppArgDef (ArgDef s e) = 
    indent $ padStringR 9 (varname s) <+> char '=' <+> format e

ppVarDef :: VarDef -> Doc
ppVarDef (VarDef s e) = 
    indent $ padStringR 9 (varname s) <+> char '=' <+> format e

ppGotoSpec :: GotoSpec -> Doc
ppGotoSpec IGoto  = text "igoto"
ppGotoSpec KGoto  = text "kgoto"
ppGotoSpec TIGoto = text "tigoto"
ppGotoSpec Goto   = text "goto"

ppPrimStmt :: PrimStmt -> Doc
ppPrimStmt (AssignS s e)     = 
    indent $ padStringR 9 (varname s) <+> char '=' <+> format e

ppPrimStmt (OpcodeS vs op es) = 
    indent $ varlist vs <+> padStringR 9 op <+> arglist (map format es)

ppPrimStmt (IfS e gspec lbl) = 
    indent $ padStringR 9 "if" <+> format e <+> ppGotoSpec gspec <+> text lbl

ppPrimStmt (LabelS lbl es) = 
    vconcat $ lbl_line : map ppPrimStmt es 
  where
    lbl_line = text lbl <> char ':' 

-- | varlist attemps to print a width 9 string, except for multiple
-- assignments where it doesn\'t care.
--
varlist :: [Var] -> Doc
varlist []  = text $ replicate 9 ' '
varlist [s] = padStringR 9 (varname s)
varlist vs  = arglist $ map (text .varname) vs


indent :: Doc -> Doc
indent = nest 2 