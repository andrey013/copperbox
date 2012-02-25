{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.Ocaml
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common Ocaml operator pretty printers with associated fixity 
-- and associativity.
--
-- The deprecated operators @(&)@ (boolean and) and @(or)@ 
-- (boolean or) are not defined.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.Ocaml
  (
  -- * Re-export DocE
    DocE(..)

  -- * Unparse a DocE
  , unparse

  -- * Literals
  , literal

  -- * Ocaml operators

  -- ** Precedence 15
  , fieldSelectorB

  -- ** Precedence 14
  , funAppB1
  , funAppB
  , notU  


  -- ** Precedence 13
  , inegateU
  , fnegateU

  -- ** Precedence 12
  , exponentB
  , lslB
  , lsrB
  , asrB

  -- ** Precedence 11
  , imultiplyB
  , fmultiplyB
  , idivideB
  , fdivideB
  , modB
  , landB
  , lorB
  , lxorB

  -- ** Precedence 10
  , iaddB
  , faddB
  , isubtractB
  , fsubtractB

  -- ** Precedence 9
  , listConsB

  -- ** Precedence 8
  , stringConcatB
  , listConcatB

  -- ** Precedence 7
  , structuralEqB
  , pointerEqB
  , structuralNotEqB
  , pointerNotEqB  
  , lessThanB
  , lessThanEqB
  , greaterThanB
  , greaterThanEqB


  -- ** Precedence 6
  , booleanAndB

  -- ** Precedence 5
  , booleanOrB

  -- ** Precedence 4
  , tupleConsB

  -- ** Precedence 3
  , vectorSetB
  , refSetB

  -- ** Precedence 1
  , semicolonB


  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr



-- | Run an unparser for Ocaml-like language expressions.
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
fieldSelectorB          :: DocE -> DocE -> DocE
fieldSelectorB          = Binary $ infixNone 15 NoSpace "."


--------------------------------------------------------------------------------
-- Precedence 14


funAppB1                :: DocE -> DocE -> DocE
funAppB1                = Binary $ infixL 14 NoSpace " "

funAppB                 :: DocE -> [DocE] -> DocE
funAppB d []            = d
funAppB d [e]           = d `funAppB1` e
funAppB d (e:es)        = d `funAppB1` e `funAppB` es


notU                    :: DocE -> DocE
notU                    = funAppB1 (literal "not")



--------------------------------------------------------------------------------
-- Precedence 13

-- | Prefix integer negation.
-- 
-- > - (prefix)
--
inegateU                :: DocE -> DocE
inegateU                = Unary $ prefix 13 NoSpace "-"


-- | Prefix floating-point negation.
-- 
-- > -. (prefix)
--
fnegateU                :: DocE -> DocE
fnegateU                = Unary $ prefix 13 NoSpace "-."


--------------------------------------------------------------------------------
-- Precedence 12

-- | Floating-point exponentiation operator.
-- 
-- > ** (infixr 12)
--
exponentB               :: DocE -> DocE -> DocE
exponentB               = Binary $ infixR 12 Space "**"


-- | Bitwise logical left-shift operator.
-- 
-- > lsl (infixr 12)
--
lslB                    :: DocE -> DocE -> DocE
lslB                    = Binary $ infixR 12 Space "lsr"


-- | Bitwise logical right-shift operator.
-- 
-- > lsr (infixr 12)
--
lsrB                    :: DocE -> DocE -> DocE
lsrB                    = Binary $ infixR 12 Space "lsr"


-- | Bitwise arithmentic right-shift operator.
-- 
-- > asr (infixr 12)
--
asrB                    :: DocE -> DocE -> DocE
asrB                    = Binary $ infixR 12 Space "asr"


--------------------------------------------------------------------------------
-- Precedence 11

-- | Integer multiplication operator.
-- 
-- > * (infixl 11)
--
imultiplyB              :: DocE -> DocE -> DocE
imultiplyB              = Binary $ infixL 11 Space "*"


-- | Floating-point multiplication operator.
-- 
-- > *. (infixl 11)
--
fmultiplyB              :: DocE -> DocE -> DocE
fmultiplyB              = Binary $ infixL 11 Space "*."


-- | Integer division operator.
-- 
-- > / (infixl 11)
--
idivideB                :: DocE -> DocE -> DocE
idivideB                = Binary $ infixL 11 Space "/"

-- | Floating-point division operator.
-- 
-- > /. (infixl 11)
--
fdivideB                :: DocE -> DocE -> DocE
fdivideB                = Binary $ infixL 11 Space "/."


-- | Integer modulus operator.
-- 
-- > mod (infixl 11)
--
modB                    :: DocE -> DocE -> DocE 
modB                    = Binary $ infixL 11 Space "mod"


-- | Logical and operator.
-- 
-- > land (infixl 11)
--
landB                   :: DocE -> DocE -> DocE
landB                   = Binary $ infixL 11 Space "land"


-- | Logical orand operator.
-- 
-- > lor (infixl 11)
--
lorB                    :: DocE -> DocE -> DocE 
lorB                    = Binary $ infixL 11 Space "lor"


-- | Logical x-or operator.
-- 
-- > lxor (infixl 11)
--
lxorB                   :: DocE -> DocE -> DocE
lxorB                   = Binary $ infixL 11 Space "lxor"


--------------------------------------------------------------------------------
-- Precedence 10


-- | Integer addition operator.
-- 
-- > + (infixl 10)
--
iaddB                   :: DocE -> DocE -> DocE
iaddB                   = Binary $ infixL 10 NoSpace "+"


-- | Floating-point addition operator.
-- 
-- > +. (infixl 10)
--
faddB                   :: DocE -> DocE -> DocE
faddB                   = Binary $ infixL 10 Space "+."



-- | Integer subtraction operator.
-- 
-- > - (infixl 6)
--
isubtractB              :: DocE -> DocE -> DocE 
isubtractB              = Binary $ infixL 10 NoSpace "-"

-- | Floating-point subtraction operator.
-- 
-- > -. (infixl 6)
--
fsubtractB              :: DocE -> DocE -> DocE
fsubtractB              = Binary $ infixL 10 Space "-."


--------------------------------------------------------------------------------
-- Precedence 9


-- | List cons operator.
-- 
-- > :: (infixr 9)
--
listConsB               :: DocE -> DocE -> DocE
listConsB               = Binary $ infixR 9 NoSpace "::"


--------------------------------------------------------------------------------
-- Precedence 8

-- | String concatenation operator.
-- 
-- > ^ (infixr 8)
--
stringConcatB           :: DocE -> DocE -> DocE
stringConcatB           = Binary $ infixR 8 NoSpace "^"


-- | List concatenation operator.
-- 
-- > @ (infixr 8)
--
listConcatB             :: DocE -> DocE -> DocE
listConcatB             = Binary $ infixR 8 NoSpace "@"


--------------------------------------------------------------------------------
-- Precedence 7

-- | Structural equality testing operator (left-associative).
--
-- > = (infixl 7)
--
structuralEqB           :: DocE -> DocE -> DocE 
structuralEqB           = Binary $ infixL 7 Space "="

-- | Physical (pointer) equality testing operator.
--
-- > == (infixl 7)
--
pointerEqB              :: DocE -> DocE -> DocE
pointerEqB              = Binary $ infixL 7 Space "=="


-- | Structural inequality testing operator.
--
-- > <> (infixl 7)
--
structuralNotEqB        :: DocE -> DocE -> DocE
structuralNotEqB        = Binary $ infixL 7 Space "<>"


-- | Physical (pointer) inequality testing operator.
--
-- > != (infixl 7)
--
pointerNotEqB           :: DocE -> DocE -> DocE  
pointerNotEqB           = Binary $ infixL 7 Space "!="


-- | Less than operator (non-associative).
-- 
-- > < (infixl 7)
--
lessThanB               :: DocE -> DocE -> DocE
lessThanB               = Binary $ infixL 7 Space "<"


-- | Less than or equal operator (non-associative).
-- 
-- > <= (infixl 7)
--
lessThanEqB             :: DocE -> DocE -> DocE
lessThanEqB             = Binary $ infixL 7 Space "<="

-- | Greater than operator (non-associative).
-- 
-- > > (infixl 7)
--
greaterThanB            :: DocE -> DocE -> DocE
greaterThanB            = Binary $ infixL 7 Space ">"


-- | Greater than or equal operator (non-associative).
-- 
-- > >= (infixl 7)
--
greaterThanEqB          :: DocE -> DocE -> DocE
greaterThanEqB          = Binary $ infixL 7 Space ">="

--------------------------------------------------------------------------------
-- Precedence 6

-- | Boolean and operator.
--
-- > && (infixl 6)
--
booleanAndB             :: DocE -> DocE -> DocE
booleanAndB             = Binary $ infixL 5 Space "&&"



--------------------------------------------------------------------------------
-- Precedence 5

-- | Boolean or operator.
--
-- > || (infixl 5)
--
booleanOrB              :: DocE -> DocE -> DocE
booleanOrB              = Binary $ infixL 5 Space "||"


--------------------------------------------------------------------------------
-- Precedence 4


-- | Tuple construction operator (comma).
--
-- > , (infix 3)
--
tupleConsB              :: DocE -> DocE -> DocE
tupleConsB              = Binary $ infixNone 4 Space ","

--------------------------------------------------------------------------------
-- Precedence 3


-- | Vector and record assignment operator.
--
-- > <- (infixr 3)
--
vectorSetB              :: DocE -> DocE -> DocE
vectorSetB              = Binary $ infixR 3 NoSpace "<-"


-- | Ref cell assignment operator.
--
-- > := (infixr 3)
--
refSetB                 :: DocE -> DocE -> DocE
refSetB              = Binary $ infixR 3 Space ":="


--------------------------------------------------------------------------------
-- Precedence 1

-- | Sequencing operator.
--
-- > ; (infixr 9)
--
semicolonB              :: DocE -> DocE -> DocE
semicolonB              = Binary $ infixR 1 Space ";"
