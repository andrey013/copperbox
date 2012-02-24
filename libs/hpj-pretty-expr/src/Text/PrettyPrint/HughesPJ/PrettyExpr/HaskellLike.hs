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
-- Expression pretty printers using Haskell\'s fixity and
-- associativity.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.HaskellLike
  (
   
  -- * Re-export the DocE type 
    DocE(..)
   
  -- * Re-export unparse    
  , unparse
    
  -- * Haskell operators

  -- ** Precedence 9
  , listIndexB 
  , composeB
  , negateU

  , addB

  , subB 
  , mulB
  , divB
  , textDivB 


  , logicalAndB
  , logicalOrB

  , powerOfB

  , modulusB


  ) where

import Text.PrettyPrint.HughesPJ.PrettyExpr
import Text.PrettyPrint.HughesPJ.PrettyExpr.HaskellOps


--------------------------------------------------------------------------------
-- Precedence 9

-- | List indexing operator.
--
-- > !! (infixl 9)
--
listIndexB              :: DocE -> DocE -> DocE
listIndexB              = Binary list_index



-- | Function composition operator.
-- 
-- > . (infixr 9)
--
composeB                :: DocE -> DocE -> DocE
composeB                = Binary fun_compose


-- | Unary negation, textual representation.
-- 
-- > negate (unary)
--
negateU                 :: DocE -> DocE
negateU                 = Unary text_negate_op



--  
addB            :: DocE -> DocE -> DocE
addB            = Binary add_op 


subB            :: DocE -> DocE -> DocE
subB            = Binary subtract_op 


mulB            :: DocE -> DocE -> DocE
mulB            = Binary multiply_op 

-- | Note this prints the @/@ operator, use
-- 'textDivB' to print @\`div\`@.
--
divB            :: DocE -> DocE -> DocE
divB            = Binary divide_op 


textDivB        :: DocE -> DocE -> DocE
textDivB        = Binary text_div_op 


logicalAndB     :: DocE -> DocE -> DocE
logicalAndB     = Binary logical_and

logicalOrB      :: DocE -> DocE -> DocE
logicalOrB      = Binary logical_or

powerOfB        :: DocE -> DocE -> DocE
powerOfB        = Binary power_of

modulusB        :: DocE -> DocE -> DocE
modulusB        = Binary text_mod_op

