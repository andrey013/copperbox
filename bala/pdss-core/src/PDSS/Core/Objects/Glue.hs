{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Objects.Glue
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - Glue.
--
--------------------------------------------------------------------------------


module PDSS.Core.Objects.Glue
  ( 

    float
  , symbol
  , int
  , print
  , send 
  , receive
  , select1
  , select2
  , route1
  , route2
  , pack1


  ) where 

import PDSS.Core.ObjectBasis

import Data.Sized.Ix                            -- package: sized types

import Prelude hiding ( print ) 





-- | Float constant.
--
-- WARNING - uses Haskell show likely to be invalid for Pd...
float :: Double -> LocObject X2 X1
float d = genLocObject "float" [PdFloat d]


-- | Symbol constant.
-- 
-- Should not contain spaces - unchecked.
--
symbol :: String -> LocObject X2 X1
symbol ss = genLocObject "symbol" [PdSymbol ss]


-- | Integer constant.
--
int :: Int -> LocObject X2 X1
int i = genLocObject "int" [PdInt i]




print :: LocObject X1 X0
print = genLocObject "print" []

-- | Farnell p. 171.
--
send :: String -> LocObject X1 X0
send name = genLocObject "send" [PdSymbol name]

-- | Farnell p. 171.
--
receive :: String -> LocObject X0 X1
receive name = genLocObject "receive" [PdSymbol name]




-- | Farnell p. 169.
--
-- Potentially @select@ is an arity family.
-- 
select1 :: PdAtom -> LocObject X1 X2
select1 a1 = genLocObject "select" [a1]

select2 :: PdAtom -> PdAtom -> LocObject X1 X3
select2 a1 a2 = genLocObject "select" [a1, a2]


-- | Farnell p. 169.
--
-- Potentially @route@ is an arity family.
-- 
-- Arg is usually a symbol.
-- 
route1 :: PdAtom -> LocObject X1 X2
route1 a1 = genLocObject "route" [a1]

route2 :: PdAtom -> PdAtom -> LocObject X1 X3
route2 a1 a2 = genLocObject "route" [a1, a2]

-- | Note - cannot handle default values at the moment...
--
pack1 :: PdType -> LocObject X1 X1
pack1 t1 = genLocObject "pack" [symType t1]


symType :: PdType -> PdAtom
symType TyFloat  = PdSymbol "f"
symType TySymbol = PdSymbol "s"



--------------------------------------------------------------------------------



