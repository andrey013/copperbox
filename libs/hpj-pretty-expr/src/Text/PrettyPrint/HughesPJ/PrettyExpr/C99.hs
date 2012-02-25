{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.C99
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common C operator pretty printers with associated fixity and
-- associativity.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.C99
  (
  -- * Unparse a DocE
    unparse

  -- * C operators
  -- ** Precedence 16

  , directSelectionU
  , indirectSelectionU
  , postIncrU
  , postDecrU


  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr


-- | Run an unparser for C-like language expressions.
-- 
-- C99 has maximum precedence of 16.
--
unparse :: DocE -> Doc
unparse = unparser $ makeUnparser 16


--------------------------------------------------------------------------------
-- Precedence 15

-- | direct selection.
-- 
-- > . (postfix 15)
--
directSelectionU        :: DocE -> DocE
directSelectionU        = Unary $ postfix 15 NoSpace "."


-- | indirect selection.
-- 
-- > -> (postfix 15)
--
indirectSelectionU      :: DocE -> DocE
indirectSelectionU      = Unary $ postfix 15 NoSpace "->"


-- | Postfix increment.
-- 
-- > ++ (postfix 15)
--
postIncrU               :: DocE -> DocE
postIncrU               = Unary $ postfix 15 NoSpace "++"

-- | Postfix decrement.
-- 
-- > - (postfix 15)
--
postDecrU               :: DocE -> DocE
postDecrU               = Unary $ postfix 15 NoSpace "--"

--------------------------------------------------------------------------------
-- 

