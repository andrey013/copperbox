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
-- Expression pretty printers using C\'s fixity and
-- associativity.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr.C99Like
  (
    
    unparse
    
  , addB

  , subB 
  , mulB
  , divB
  , textDivB 


  , negateU
  , logicalAndB
  , logicalOrB

  , powerOfB

  , modulusB


  ) where

import Text.PrettyPrint.HughesPJ.PrettyExpr
import Text.PrettyPrint.HughesPJ.PrettyExpr.C99Ops

--  
addB            :: DocE -> DocE -> DocE
addB            = Binary add_op 


subB            :: DocE -> DocE -> DocE
subB            = Binary sub_op 


mulB            :: DocE -> DocE -> DocE
mulB            = Binary mul_op 

-- | Note this prints the @/@ operator, use
-- 'textDivB' to print @\`div\`@.
--
divB            :: DocE -> DocE -> DocE
divB            = Binary div_op 


textDivB        :: DocE -> DocE -> DocE
textDivB        = Binary text_div_op 

negateU         :: DocE -> DocE
negateU         = Unary unary_negate


logicalAndB     :: DocE -> DocE -> DocE
logicalAndB     = Binary logical_and

logicalOrB      :: DocE -> DocE -> DocE
logicalOrB      = Binary logical_or

powerOfB        :: DocE -> DocE -> DocE
powerOfB        = Binary power_of

modulusB        :: DocE -> DocE -> DocE
modulusB        = Binary modulus_op

