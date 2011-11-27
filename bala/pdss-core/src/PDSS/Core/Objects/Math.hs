{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Objects.Math
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - Math.
--
--------------------------------------------------------------------------------


module PDSS.Core.Objects.Math
  ( 

    mtof
  , ftom

  ) where 

import PDSS.Core.ObjectBasis


import Data.Sized.Ix                            -- package: sized-types



mtof :: LocObject X1 X1
mtof = genLocObject "mtof" []


ftom :: LocObject X1 X1
ftom = genLocObject "from" []

