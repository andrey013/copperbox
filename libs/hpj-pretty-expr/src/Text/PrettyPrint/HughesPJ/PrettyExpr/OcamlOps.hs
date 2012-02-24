{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.OcamlOps
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pre-defined common Ocaml operators with associated fixity and
-- associativity.
--
-- The deprecated operators @(&)@ (boolean and) and @(or)@ 
-- (boolean or) are not defined.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.OcamlOps 
  (
  -- * Unparse a DocE
    unparse

  -- * Ocaml operators

  -- ** Precedence 15
  , field_selector

  -- ** Precedence 13
  , negate_int
  , negate_float

  -- ** Precedence 12
  , exponent_op
  , lsl_op
  , lsr_op
  , asr_op

  -- ** Precedence 11
  , multiply_int
  , multiply_float
  , divide_int
  , divide_float
  , mod_op
  , land_op
  , lor_op
  , lxor_op

  -- ** Precedence 10
  , add_int
  , add_float
  , subtract_int
  , subtract_float

  -- ** Precedence 9
  , list_cons

  -- ** Precedence 8
  , string_concat
  , list_concat

  -- ** Precedence 7
  , structural_equal
  , pointer_equal
  , structural_not_equal
  , pointer_not_equal  
  , less_than
  , less_than_equal
  , greater_than
  , greater_than_equal


  -- ** Precedence 6
  , boolean_and

  -- ** Precedence 5
  , boolean_or

  -- ** Precedence 4
  , tuple_cons_op

  -- ** Precedence 3
  , vector_set_op
  , ref_set_op

  -- ** Precedence 1
  , semicolon_op


  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr



-- | Run an unparser for Ocaml-like languages.
-- 
-- Ocaml appears to have a maximum precedence of 16 
-- (17 levels counting from 0), see section 6.7 of the reference
-- Manual.
--
unparse :: DocE -> Doc
unparse = unparser $ makeUnparser 16


--------------------------------------------------------------------------------
-- Precedence 15

-- | Record field selection operator.
-- 
-- > . (infix 15)
--
field_selector          :: Rator
field_selector          = infixNone 15 "."



--------------------------------------------------------------------------------
-- Precedence 13

-- | Prefix integer negation.
-- 
-- > - (prefix)
--
negate_int              :: Rator
negate_int              = prefix 13 "-"


-- | Prefix floating-point negation.
-- 
-- > -. (prefix)
--
negate_float            :: Rator
negate_float            = prefix 13 "-."


--------------------------------------------------------------------------------
-- Precedence 12

-- | Floating-point exponentiation operator.
-- 
-- > ** (infixr 12)
--
exponent_op              :: Rator
exponent_op              = infixR 12 "**"


-- | Bitwise logical left-shift operator.
-- 
-- > lsl (infixr 12)
--
lsl_op                  :: Rator
lsl_op                  = infixR 12 "lsr"


-- | Bitwise logical right-shift operator.
-- 
-- > lsr (infixr 12)
--
lsr_op                  :: Rator
lsr_op                  = infixR 12 "lsr"


-- | Bitwise arithmentic right-shift operator.
-- 
-- > asr (infixr 12)
--
asr_op                  :: Rator
asr_op                  = infixR 12 "asr"


--------------------------------------------------------------------------------
-- Precedence 11

-- | Integer multiplication operator.
-- 
-- > * (infixl 11)
--
multiply_int             :: Rator
multiply_int             = infixL 11 "*"

-- | Floating-point multiplication operator.
-- 
-- > *. (infixl 11)
--
multiply_float           :: Rator
multiply_float           = infixL 11 "*."


-- | Integer division operator.
-- 
-- > / (infixl 11)
--
divide_int              :: Rator
divide_int              = infixL 11 "/"

-- | Floating-point division operator.
-- 
-- > /. (infixl 11)
--
divide_float            :: Rator
divide_float            = infixL 11 "/."


-- | Integer modulus operator.
-- 
-- > mod (infixl 11)
--
mod_op                  :: Rator
mod_op                  = infixL 11 "mod"


-- | Logical and operator.
-- 
-- > land (infixl 11)
--
land_op                 :: Rator
land_op                 = infixL 11 "land"


-- | Logical orand operator.
-- 
-- > lor (infixl 11)
--
lor_op                  :: Rator
lor_op                  = infixL 11 "lor"


-- | Logical x-or operator.
-- 
-- > lxor (infixl 11)
--
lxor_op                 :: Rator
lxor_op                 = infixL 11 "lxor"


--------------------------------------------------------------------------------
-- Precedence 10


-- | Integer addition operator.
-- 
-- > + (infixl 10)
--
add_int                 :: Rator
add_int                 = infixL 10 "+"


-- | Floating-point addition operator.
-- 
-- > +. (infixl 10)
--
add_float               :: Rator
add_float               = infixL 10 "+."



-- | Integer subtraction operator.
-- 
-- > - (infixl 6)
--
subtract_int            :: Rator
subtract_int            = infixL 10 "-"

-- | Floating-point subtraction operator.
-- 
-- > -. (infixl 6)
--
subtract_float          :: Rator
subtract_float          = infixL 10 "-."


--------------------------------------------------------------------------------
-- Precedence 9


-- | List cons operator.
-- 
-- > :: (infixr 9)
--
list_cons               :: Rator
list_cons               = infixR 9 "::"


--------------------------------------------------------------------------------
-- Precedence 8

-- | String concatenation operator.
-- 
-- > ^ (infixr 8)
--
string_concat             :: Rator
string_concat             = infixR 8 "^"

-- | List concatenation operator.
-- 
-- > @ (infixr 8)
--
list_concat             :: Rator
list_concat             = infixR 8 "@"


--------------------------------------------------------------------------------
-- Precedence 7

-- | Structural equality testing operator (left-associative).
--
-- > = (infixl 7)
--
structural_equal        :: Rator
structural_equal        = infixL 7 "="

-- | Physical (pointer) equality testing operator.
--
-- > == (infixl 7)
--
pointer_equal           :: Rator
pointer_equal           = infixL 7 "=="


-- | Structural inequality testing operator.
--
-- > <> (infixl 7)
--
structural_not_equal    :: Rator
structural_not_equal    = infixL 7 "<>"


-- | Physical (pointer) inequality testing operator.
--
-- > != (infixl 7)
--
pointer_not_equal       :: Rator
pointer_not_equal       = infixL 7 "!="


-- | Less than operator (non-associative).
-- 
-- > < (infixl 7)
--
less_than               :: Rator
less_than               = infixL 7 "<"


-- | Less than or equal operator (non-associative).
-- 
-- > <= (infixl 7)
--
less_than_equal         :: Rator
less_than_equal         = infixL 7 "<="

-- | Greater than operator (non-associative).
-- 
-- > > (infixl 7)
--
greater_than            :: Rator
greater_than            = infixL 7 ">"


-- | Greater than or equal operator (non-associative).
-- 
-- > >= (infixl 7)
--
greater_than_equal      :: Rator
greater_than_equal      = infixL 7 ">="

--------------------------------------------------------------------------------
-- Precedence 6

-- | Boolean and operator.
--
-- > && (infixl 6)
--
boolean_and             :: Rator
boolean_and             = infixL 5 "&&"



--------------------------------------------------------------------------------
-- Precedence 5

-- | Boolean or operator.
--
-- > || (infixl 5)
--
boolean_or              :: Rator
boolean_or              = infixL 5 "||"


--------------------------------------------------------------------------------
-- Precedence 4


-- | Tuple construction operator (comma).
--
-- > , (infix 3)
--
tuple_cons_op           :: Rator
tuple_cons_op           = infixNone 4 ","

--------------------------------------------------------------------------------
-- Precedence 3


-- | Vector and record assignment operator.
--
-- > <- (infixr 3)
--
vector_set_op           :: Rator
vector_set_op           = infixR 3 "<-"


-- | Ref cell assignment operator.
--
-- > := (infixr 3)
--
ref_set_op              :: Rator
ref_set_op              = infixR 3 ":="


--------------------------------------------------------------------------------
-- Precedence 1

-- | Sequencing operator.
--
-- > ; (infixr 9)
--
semicolon_op            :: Rator
semicolon_op            = infixR 1 ";"
