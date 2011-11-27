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

  ) where 

import PDSS.Core.ObjectBasis

import Data.Sized.Ix                            -- package: sized types

import Prelude hiding ( print ) 





-- | Float constant.
--
-- WARNING - uses Haskell show likely to be invalid for Pd...
float :: Double -> LocObject X2 X1
float d = genLocObject "float" [show d]


-- | Symbol constant.
-- 
-- Should not contain spaces - unchecked.
--
symbol :: String -> LocObject X2 X1
symbol ss = genLocObject "symbol" [ss]


-- | Integer constant.
--
int :: Int -> LocObject X2 X1
int i = genLocObject "int" [show i]




print :: LocObject X1 X0
print = genLocObject "print" []

-- | Farnell p. 171.
--
send :: String -> LocObject X1 X0
send name = genLocObject "send" [name]

-- | Farnell p. 171.
--
receive :: String -> LocObject X0 X1
receive name = genLocObject "receive" [name]



--------------------------------------------------------------------------------



