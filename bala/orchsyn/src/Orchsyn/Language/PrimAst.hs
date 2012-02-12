{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.PrimAst
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (Generalized newtype deriving, TypeFamilies...)
--
-- Low level AST - subset of Csound.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.PrimAst
  (
   
    OrchDef(..) 
  , Global(..)
  , InstDef(..)
  , ArgDef(..)
  , VarDef(..)
  , Label
  , GotoSpec(..)
  , PrimStmt(..)

  ) where

import Orchsyn.Language.Expr


import Data.Generics	( Data, Typeable )


data OrchDef = OrchDef
      { globals         :: [Global]
      , instrs          :: [InstDef]
      }
  deriving (Eq, Ord, Show, Data, Typeable)


-- | Glabls can be assignment to global variables
-- or initialization statements e.g. for @zak@.
--
data Global = AssignG Var DExpr
            | OpcodeG Var String [DExpr]
  deriving (Eq, Ord, Show, Data, Typeable)


-- | Inst_name might actually be a numeric literal for Csound.
data InstDef = InstDef
      { inst_num        :: Int
      , arg_defs        :: [ArgDef]
      , var_defs        :: [VarDef]
      , body_stmts      :: [PrimStmt]
      }
  deriving (Eq, Ord, Show, Data, Typeable)


data ArgDef = ArgDef Var DExpr
  deriving (Eq, Ord, Show, Data, Typeable)

data VarDef = VarDef Var DExpr
  deriving (Eq, Ord, Show, Data, Typeable)

data GotoSpec = IGoto | KGoto | TIGoto | Goto
  deriving (Eq, Ord, Show, Data, Typeable)

type Label = String


-- | Opcodes can assign to 0 (e.g. @out@), 1 (the common case) 
-- or more variables (e.g. @xyin@ assigns to two variables).
--
data PrimStmt = AssignS Var DExpr
              | OpcodeS [Var] String [DExpr]
              | IfS  DExpr GotoSpec Label
              | LabelS Label [PrimStmt]
  deriving (Eq, Ord, Show, Data, Typeable)


