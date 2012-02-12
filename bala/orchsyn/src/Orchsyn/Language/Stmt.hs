{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Stmt
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (Generalized newtype deriving)
--
-- Statements.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Stmt
  (
   
    Stmt(..)

  , stmtToPrimStmt

  ) where

import Orchsyn.Language.Expr
import qualified Orchsyn.Language.PrimAst as Prim


import Data.Generics	( Data, Typeable )


data Stmt = AssignS Var DExpr	 
          | OpcodeS [Var] String [DExpr]
  deriving (Eq, Ord, Show, Data, Typeable)



stmtToPrimStmt :: Stmt -> Prim.PrimStmt
stmtToPrimStmt (AssignS v e)            = Prim.AssignS v e
stmtToPrimStmt (OpcodeS vs opco es)     = Prim.OpcodeS vs opco es