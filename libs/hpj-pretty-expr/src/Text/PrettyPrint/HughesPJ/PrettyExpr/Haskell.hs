{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr.Haskell
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common Haskell operator pretty printerss with associated fixity 
-- and associativity. 
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.Haskell
  (

  -- * Unparse a DocE
    unparse

  -- * Haskell operators

  -- ** Precedence 9
  , listIndexB
  , composeB
  , negateU

  -- ** Precedence 8
  , powerOfB
  , powerOfFracB
  , expLogB

  -- ** Precedence 7
  , multiplyB  
  , divideB
  , divB
  , modB
  , remB
  , quotB

  -- ** Precedence 6
  , addB
  , subtractB 

  -- ** Precedence 5
  , consB
  , concatB

  -- ** Precedence 4
  , equalB
  , notEqualB
  , lessThanB
  , lessThanEqB
  , greaterThanB
  , greaterThanEqB
  , elemB
  , notElemB


  -- * Precedence 3
  , logicalAndB

  -- * Precedence 2
  , logicalOrB

  -- * Precedence 1
  , monadicSeqB
  , monadicBindB

  -- * Precedence 0
  , applyB
  , strictApplyB
  , seqB

  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJ.PrettyExpr

-- | Run an unparser for Haskell-like language expressions.
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
listIndexB              :: DocE -> DocE -> DocE
listIndexB              = Binary $ infixL 9 NoSpace "!!"


-- | Function composition operator.
-- 
-- > . (infixr 9)
--
composeB                :: DocE -> DocE -> DocE
composeB                = Binary $ infixR 9 Space "."




-- | Unary negation, textual representation.
-- 
-- > negate (unary)
--
negateU                 :: DocE -> DocE
negateU                 = Unary $ prefix 9 Space "negate"

--------------------------------------------------------------------------------
-- Precedence 8

-- | Power-of operator (Num constraint), non-negative.
-- 
-- > ^ (infixr 8)
--
powerOfB                :: DocE -> DocE -> DocE
powerOfB                = Binary $ infixR 8 Space "^"


-- | Power-of operator (Fractional constraint), negative powers 
-- allowed .
-- 
-- > ^^ (infixr 8)
--
powerOfFracB            :: DocE -> DocE -> DocE
powerOfFracB            = Binary $ infixR 8 Space "^^"


-- | Exponent log (?) operator.
--
--
-- > ** (infixr 8)
--
-- > x ** y == exp (log x * y)
--  
expLogB                 :: DocE -> DocE -> DocE
expLogB                 = Binary $ infixR 8 Space "**"

--------------------------------------------------------------------------------
-- Precedence 7

-- | Multiplication operator.
-- 
-- > * (infixl 7)
--
multiplyB               :: DocE -> DocE -> DocE
multiplyB               = Binary $ infixL 7 Space "*"


-- | Division operator (Fractional).
-- 
-- > / (infixl 7)
--
divideB                 :: DocE -> DocE -> DocE
divideB                 = Binary $ infixL 7 Space "/"


-- | Division operator, (Int) textual representation.
-- 
-- > `div` (infixl 7)
--
divB                    :: DocE -> DocE -> DocE
divB                    = Binary $ infixL 7 Space "`div`"


-- | Modulus operator, textual representation.
-- 
-- > `mod` (infixl 7)
--
modB                    :: DocE -> DocE -> DocE
modB                    = Binary $ infixL 7 Space "`mod`"


-- | Remainder operator, textual representation.
-- 
-- > `rem` (infixl 7)
--
remB                    :: DocE -> DocE -> DocE
remB                    = Binary $ infixL 7 Space "`rem`"

-- | Quotient operator, textual representation.
-- 
-- > `quot` (infixl 7)
--
quotB                   :: DocE -> DocE -> DocE
quotB                   = Binary $ infixL 7 Space "`quot`"


--------------------------------------------------------------------------------
-- Precedence 6


-- | Addition operator.
-- 
-- > + (infixl 6)
--
addB                    :: DocE -> DocE -> DocE
addB                    = Binary $ infixL 6 Space "+"


-- | Subtraction operator.
-- 
-- > - (infixl 6)
--
subtractB               :: DocE -> DocE -> DocE
subtractB               = Binary $ infixL 6 Space "-"

--------------------------------------------------------------------------------
-- Precedence 5

-- | Cons operator (Lists).
-- 
-- > : (infixr 5)
--
consB                   :: DocE -> DocE -> DocE
consB                   = Binary $ infixR 5 NoSpace ":"

-- | Concatenation operator (Lists).
-- 
-- > ++ (infixr 5)
--
concatB                 :: DocE -> DocE -> DocE
concatB                 = Binary $ infixR 5 NoSpace "++"


--------------------------------------------------------------------------------
-- Precedence 4


-- | Equality testing operator (non-associative).
-- 
-- > == (infix 4)
--
equalB                  :: DocE -> DocE -> DocE
equalB                  = Binary $ infixNone 4 Space "=="

-- | Inequality testing operator (non-associative).
-- 
-- > /= (infix 4)
--
notEqualB               :: DocE -> DocE -> DocE
notEqualB               = Binary $ infixNone 4 Space "/="


-- | Less than operator (non-associative).
-- 
-- > < (infix 4)
--
lessThanB               :: DocE -> DocE -> DocE
lessThanB               = Binary $ infixNone 4 Space "<"


-- | Less than or equal operator (non-associative).
-- 
-- > <= (infix 4)
--
lessThanEqB             :: DocE -> DocE -> DocE
lessThanEqB             = Binary $ infixNone 4 Space "<="

-- | Greater than operator (non-associative).
-- 
-- > > (infix 4)
--
greaterThanB            :: DocE -> DocE -> DocE
greaterThanB            = Binary $ infixNone 4 Space ">"


-- | Greater than or equal operator (non-associative).
-- 
-- > >= (infix 4)
--
greaterThanEqB          :: DocE -> DocE -> DocE
greaterThanEqB          = Binary $ infixNone 4 Space ">="


-- | Elem - list membership operator, textual representation.
-- 
-- > `elem` (infix 4)
--
elemB                   :: DocE -> DocE -> DocE
elemB                   = Binary $ infixNone 4 Space "`elem`"

-- | notElem - list non-membership operator, textual 
-- representation.
-- 
-- > `notElem` (infix 4)
--
notElemB                :: DocE -> DocE -> DocE
notElemB                = Binary $ infixNone 4 Space "`notElem`"

--------------------------------------------------------------------------------
-- Precedence 3

-- | Logical and operator.
-- 
-- > && (infixr 3)
--
logicalAndB             :: DocE -> DocE -> DocE
logicalAndB             = Binary $ infixR 3 Space "&&"



--------------------------------------------------------------------------------
-- Precedence 2

-- | Logical or operator.
-- 
-- > || (infixr 2)
--
logicalOrB              :: DocE -> DocE -> DocE
logicalOrB              = Binary $ infixR 2 Space "||"


--------------------------------------------------------------------------------
-- Precedence 1


-- | Monadic sequencing operator.
-- 
-- > >> (infixl 1)
--
monadicSeqB             :: DocE -> DocE -> DocE
monadicSeqB             = Binary $ infixL 1 Space ">>"


-- | Monadic bind operator.
-- 
-- > >>= (infixl 1)
--
monadicBindB            :: DocE -> DocE -> DocE
monadicBindB            = Binary $ infixL 1 Space ">>="

--------------------------------------------------------------------------------
-- Precedence 0

-- | Function application operator.
-- 
-- > $ (infixr 0)
--
applyB                  :: DocE -> DocE -> DocE
applyB                  = Binary $ infixR 0 Space "$"


-- | Strict function application operator.
-- 
-- > $! (infixr 0)
--
strictApplyB            :: DocE -> DocE -> DocE
strictApplyB            = Binary $ infixR 0 Space "$!"


-- | seq binary operator - forces left-hand side, returns
-- right-hand side.
-- 
-- > `seq` (infixr 0)
--
seqB                    :: DocE -> DocE -> DocE
seqB                    = Binary $ infixR 0 Space "`seq`"





