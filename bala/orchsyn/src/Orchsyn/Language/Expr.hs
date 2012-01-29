{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Expr
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (Generalized newtype deriving)
--
-- Expressions...
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Expr
  (
    
    Expr(..)

  ) where

import Orchsyn.Utils.PrettyExpr
import Orchsyn.Utils.PrettyExtras


import Data.Generics	( Data, Typeable )
import Text.PrettyPrint.HughesPJ

-- | String data is used by @soundin@ for example.
--
data Expr = VarE String
          | FloatE  Double
          | StringE String    
          | IntE    Int
          | BinE Rator Expr Expr
          | UnaryE  Rator Expr 
          | FunCallE String Expr
  deriving (Eq, Ord, Show, Data, Typeable)


instance Num Expr where
  (+)     = BinE plus_op
  (-)     = BinE minus_op
  (*)     = BinE mult_op
  abs     = FunCallE "abs"
  negate  = UnaryE unary_negate
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = FloatE $ fromInteger i


instance Fractional Expr where
  (/)     = BinE divide_op
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = FloatE $ fromRational d


instance Format Expr where
  format = unparse . buildExpr


buildExpr :: Expr -> DocExpr
buildExpr (VarE s)       = Atom $ text s
buildExpr (FloatE d)     = Atom $ dtrunc d
buildExpr (StringE s)    = Atom $ doubleQuotes $ text s
buildExpr (IntE i)       = Atom $ int i
buildExpr (BinE op a b)  = Binary (buildExpr a) op (buildExpr b)
buildExpr (UnaryE op a)  = Unary op (buildExpr a)
buildExpr (FunCallE s a) = Atom $ text s <> parens (format a)


