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

import Prelude ( ($) )



infixr 3 &&

(&&) :: Expr rate -> Expr rate -> Expr rate
(&&) = liftE2 $ BinE logical_and

infixr 2 ||


(||) :: Expr rate -> Expr rate -> Expr rate
(||) = liftE2 $ BinE logical_or

infixr 8 ^

(^)  :: Expr rate -> Expr rate -> Expr rate
(^)  = liftE2 $ BinE power_of


infixl 7 %

(%) :: Expr rate -> Expr rate -> Expr rate
(%) = liftE2 $ BinE modulus_op

