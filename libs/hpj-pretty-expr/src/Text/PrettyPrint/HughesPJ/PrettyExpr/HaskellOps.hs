{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.HaskellOps
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pre-defined common Haskell operators with associated fixity 
-- and associativity. 
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.HaskellOps 
  (

  -- * Unparse a DocE
    unparse

  -- * Haskell operators

  -- ** Precedence 9
  , list_index
  , fun_compose
  , text_negate_op

  -- ** Precedence 8
  , power_of
  , power_of_frac
  , exp_log

  -- ** Precedence 7
  , multiply_op  
  , divide_op
  , text_div_op
  , text_mod_op
  , text_rem_op
  , text_quot_op

  -- ** Precedence 6
  , add_op
  , subtract_op 

  -- ** Precedence 5
  , cons_op
  , concat_op

  -- ** Precedence 4
  , equal_op
  , not_equal_op
  , less_than
  , less_than_equal
  , greater_than
  , greater_than_equal
  , text_elem_op
  , text_not_elem_op


  -- * Precedence 3
  , logical_and

  -- * Precedence 2
  , logical_or

  -- * Precedence 1
  , monadic_seq
  , monadic_bind

  -- * Precedence 0
  , apply_op
  , strict_apply_op
  , text_seq_op

  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr

-- | Run an unparser for Haskell-like languages.
-- 
-- Haskell has maximum precedence of 9.
--
unparse :: DocE -> Doc
unparse = unparser $ makeUnparser 9


--------------------------------------------------------------------------------
-- Precedence 9

-- | List indexing operator.
--
-- > !! (infixl 9)
--
list_index              :: Rator
list_index              = infixL 9 "!!"


-- | Function composition operator.
-- 
-- > . (infixr 9)
--
fun_compose             :: Rator
fun_compose             = infixR 9 "."




-- | Unary negation, textual representation.
-- 
-- > negate (unary)
--
text_negate_op          :: Rator
text_negate_op           = prefix 9 "negate"

--------------------------------------------------------------------------------
-- Precedence 8

-- | Power-of operator (Num constraint), non-negative.
-- 
-- > ^ (infixr 8)
--
power_of                :: Rator
power_of                = infixR 8 "^"

-- | Power-of operator (Fractional constraint), negative powers 
-- allowed .
-- 
-- > ^^ (infixr 8)
--
power_of_frac           :: Rator
power_of_frac           = infixR 8 "^^"


-- | Exponent log (?) operator.
--
--
-- > ** (infixr 8)
--
-- > x ** y == exp (log x * y)
--  
exp_log                 :: Rator
exp_log                 = infixR 8 "**"

--------------------------------------------------------------------------------
-- Precedence 7

-- | Multiplication operator.
-- 
-- > * (infixl 7)
--
multiply_op             :: Rator
multiply_op             = infixL 7 "*"


-- | Division operator (Fractional).
-- 
-- > / (infixl 7)
--
divide_op               :: Rator
divide_op               = infixL 7 "/"


-- | Division operator, (Int) textual representation.
-- 
-- > `div` (infixl 7)
--
text_div_op             :: Rator
text_div_op             = infixL 7 "`div`"


-- | Modulus operator, textual representation.
-- 
-- > `mod` (infixl 7)
--
text_mod_op             :: Rator
text_mod_op             = infixL 7 "`mod`"


-- | Remainder operator, textual representation.
-- 
-- > `rem` (infixl 7)
--
text_rem_op             :: Rator
text_rem_op             = infixL 7 "`rem`"

-- | Quotient operator, textual representation.
-- 
-- > `quot` (infixl 7)
--
text_quot_op            :: Rator
text_quot_op            = infixL 7 "`quot`"


--------------------------------------------------------------------------------
-- Precedence 6


-- | Addition operator.
-- 
-- > + (infixl 6)
--
add_op                  :: Rator
add_op                  = infixL 6 "+"


-- | Subtraction operator.
-- 
-- > - (infixl 6)
--
subtract_op             :: Rator
subtract_op             = infixL 6 "-"

--------------------------------------------------------------------------------
-- Precedence 5

-- | Cons operator (Lists).
-- 
-- > : (infixr 5)
--
cons_op                 :: Rator
cons_op                 = infixR 5 ":"

-- | Concatenation operator (Lists).
-- 
-- > ++ (infixr 5)
--
concat_op               :: Rator
concat_op               = infixR 5 "++"


--------------------------------------------------------------------------------
-- Precedence 4


-- | Equality testing operator (non-associative).
-- 
-- > == (infix 4)
--
equal_op                :: Rator
equal_op                = infixNone 4 "=="

-- | Inequality testing operator (non-associative).
-- 
-- > /= (infix 4)
--
not_equal_op            :: Rator
not_equal_op            = infixNone 4 "/="


-- | Less than operator (non-associative).
-- 
-- > < (infix 4)
--
less_than               :: Rator
less_than               = infixNone 4 "<"


-- | Less than or equal operator (non-associative).
-- 
-- > <= (infix 4)
--
less_than_equal         :: Rator
less_than_equal         = infixNone 4 "<="

-- | Greater than operator (non-associative).
-- 
-- > > (infix 4)
--
greater_than            :: Rator
greater_than            = infixNone 4 ">"


-- | Greater than or equal operator (non-associative).
-- 
-- > >= (infix 4)
--
greater_than_equal      :: Rator
greater_than_equal      = infixNone 4 ">="


-- | Elem - list membership operator, textual representation.
-- 
-- > `elem` (infix 4)
--
text_elem_op            :: Rator
text_elem_op            = infixNone 4 "`elem`"

-- | notElem - list non-membership operator, textual 
-- representation.
-- 
-- > `notElem` (infix 4)
--
text_not_elem_op        :: Rator
text_not_elem_op        = infixNone 4 "`notElem`"

--------------------------------------------------------------------------------
-- Precedence 3

-- | Logical and operator.
-- 
-- > && (infixr 3)
--
logical_and             :: Rator
logical_and             = infixR 3 "&&"



--------------------------------------------------------------------------------
-- Precedence 2

-- | Logical or operator.
-- 
-- > || (infixr 2)
--
logical_or              :: Rator
logical_or              = infixR 2 "||"


--------------------------------------------------------------------------------
-- Precedence 1


-- | Monadic sequencing operator.
-- 
-- > >> (infixl 1)
--
monadic_seq             :: Rator
monadic_seq             = infixL 1 ">>"


-- | Monadic bind operator.
-- 
-- > >>= (infixl 1)
--
monadic_bind            :: Rator
monadic_bind            = infixL 1 ">>="

--------------------------------------------------------------------------------
-- Precedence 0

-- | Function application operator.
-- 
-- > $ (infixr 0)
--
apply_op                :: Rator
apply_op                = infixR 0 "$"


-- | Strict function application operator.
-- 
-- > $! (infixr 0)
--
strict_apply_op         :: Rator
strict_apply_op         = infixR 0 "$!"


-- | seq binary operator - forces left-hand side, returns
-- right-hand side.
-- 
-- > `seq` (infixr 0)
--
text_seq_op             :: Rator
text_seq_op             = infixR 0 "`seq`"





