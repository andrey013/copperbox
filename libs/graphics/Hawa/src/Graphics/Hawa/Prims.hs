{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Hawa.Prims
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- primitive ops
--
--------------------------------------------------------------------------------

module Graphics.Hawa.Prims where

import Graphics.Hawa.Unit

-- data TyRep = Em     -- postscript unit 

data PrimOp = 
        Pop
      -- Path ops  
      | Moveto            Unit    Unit
      | Lineto            Unit    Unit
      | Closepath
      -- paint ops
      | Stroke
  deriving (Eq,Show)      