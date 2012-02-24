{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.C99Ops
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pre-defined common C operators (C99) with associated fixity and
-- associativity.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.C99Ops 
  (
  -- * Unparse a DocE
    unparse

  -- * C operators
  -- ** Precedence 16

  , direct_selection
  , indirect_selection
  , post_incr
  , post_decr

  -- ** Precedence 1
  , sequential_evaluation 

  , add_op
  , sub_op 
  , mul_op
  , div_op
  , text_div_op

  , unary_negate
  , logical_and
  , logical_or
  , power_of

  , modulus_op

  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr

-- | Run an unparser for C-like languages.
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
direct_selection        :: Rator
direct_selection        = postfix 15 "."


-- | indirect selection.
-- 
-- > -> (postfix 15)
--
indirect_selection      :: Rator
indirect_selection      = postfix 15 "->"


-- | Postfix increment.
-- 
-- > ++ (postfix 15)
--
post_incr               :: Rator
post_incr               = postfix 15 "++"

-- | Postfix decrement.
-- 
-- > - (postfix 15)
--
post_decr               :: Rator
post_decr               = postfix 15 "--"

--------------------------------------------------------------------------------
-- 


-- | Sequential evaluation.
-- 
-- > , (infixl 1)
--
sequential_evaluation   :: Rator
sequential_evaluation   = infixL 15 ","


--  
add_op         :: Rator
add_op         = infixL 6 "+"

sub_op        :: Rator
sub_op        = infixL 6 "-"

mul_op         :: Rator
mul_op         = infixL 7 "*"


div_op       :: Rator
div_op       = infixL 7 "/"

text_div_op         :: Rator
text_div_op         = infixL 7 "`div`"

unary_negate    :: Rator
unary_negate    = prefix 9 "-"

logical_and     :: Rator
logical_and     = infixL 3 "&&"

logical_or      :: Rator
logical_or      = infixL 2 "||"

power_of        :: Rator
power_of        = infixL 4 "^"


modulus_op      :: Rator
modulus_op      = infixL 7 "%"