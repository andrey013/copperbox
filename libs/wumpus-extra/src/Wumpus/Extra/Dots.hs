{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Dots
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Dots
  ( 

  -- * Dots
    dotX
  , dotPlus
  ) where



import Wumpus.Core
import Wumpus.Geometry

-- 

-- Hmm these dots aren't very efficient being Pictures...

dotX :: (Ord u, Floating u, Real u) => Picture u
dotX = multi [ ostroke () $ lineSegmentToPath ls1
             , ostroke () $ lineSegmentToPath ls2 ]
  where
    ls1   = rotate (pi/6) $ vlineSegment 4 $ P2 0 (-2)
    ls2   = reflectX ls1


dotPlus :: (Ord u, Floating u, Real u) => Picture u
dotPlus = multi [ ostroke () $ lineSegmentToPath ls1
                , ostroke () $ lineSegmentToPath ls2 ]
  where
    ls1   = vlineSegment 4 $ P2 0 (-2)
    ls2   = hlineSegment 4 $ P2 (-2) 0