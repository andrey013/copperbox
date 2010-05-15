{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Additions due to be added to Wumpus.Core
-- (or maybe not...)
--
--------------------------------------------------------------------------------


module Wumpus.Extra.CoreAdditions
  (

    zeroPicture
  , centeredAt

  ) where


import Wumpus.Core

zeroPicture :: Num u => Picture u
zeroPicture = blankPicture (BBox zeroPt zeroPt)

centeredAt :: (Horizontal a, Vertical a, Move a, Composite a, Blank a, 
               Fractional u, u ~ PUnit a) 
           => a -> Point2 u -> a
centeredAt p pt = p -@- (blank 0 0 `at` pt) 

