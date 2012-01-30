{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Operators
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Builtin Csound operators.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Operators
  (

    (&&)
  , (||)   
  , (^)
  , (%)

  ) where

import Orchsyn.Language.Expr
import Orchsyn.Utils.PrettyExpr

import Prelude ()



infixr 3 &&

(&&) :: Expr -> Expr -> Expr
(&&) = BinE logical_and

infixr 2 ||


(||) :: Expr -> Expr -> Expr
(||) = BinE logical_or

infixr 8 ^

(^)  :: Expr -> Expr -> Expr
(^)  = BinE power_of


infixl 7 %

(%) :: Expr -> Expr -> Expr
(%) = BinE modulus_op

