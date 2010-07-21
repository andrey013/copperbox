{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.BasicAdditions
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Candidates for moving to Wumpus-Basic
-- 
--------------------------------------------------------------------------------

module Wumpus.PSC.BasicAdditions
  (

    mveloH
  , drawAt

  ) where

import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.Utils.HList

import Control.Monad

mveloH :: Monad m => (a -> m (H b)) -> [a] -> m (H b)
mveloH mf = step id 
  where
    step acc []     = return acc
    step acc (x:xs) = mf x >>= \a -> step (acc . a) xs
 

-- name ?
--
drawAt :: (Monad m , CoordScaleM m ux uy u) 
       => GraphicF u -> (ux,uy) -> m (Graphic u)
drawAt gf (x,y) = liftM gf $ coordScale (x,y)

