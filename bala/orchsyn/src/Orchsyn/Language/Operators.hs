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
-- Operators.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Operators
  (
   
    (^)
  ) where

import Orchsyn.Language.Expr
import Orchsyn.Utils.PrettyExpr

import Prelude hiding ( (^) )

infixr 8 ^

(^)  :: Expr -> Expr -> Expr
(^)  = BinE power_of


