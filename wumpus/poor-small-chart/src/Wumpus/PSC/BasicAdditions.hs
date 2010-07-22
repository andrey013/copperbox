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
  , textlineRect
  , TextlineRectDisplace
  , frameWest
  , frameNorth

  ) where

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.Utils.HList

import Control.Monad



mveloH :: Monad m => (a -> m (H b)) -> [a] -> m (H b)
mveloH mf = step id 
  where
    step acc []     = return acc
    step acc (x:xs) = mf x >>= \a -> step (acc . a) xs
 

-- This one is a problem - its useful but adding it to Basic
-- will introduce a dependency between the Graphic and 
-- CoordScaleMonad modules.
--
drawAt :: (Monad m , CoordScaleM m ux uy u) 
       => GraphicF u -> (ux,uy) -> m (Graphic u)
drawAt gf (x,y) = liftM gf $ coordScale (x,y)



textlineRect :: Fractional u 
             => (DRGB,FontAttr) -> String -> (Rectangle u, GraphicF u)
textlineRect (rgb,attr) text  = 
    (Rectangle text_width text_height, wrapG . textlabel (rgb,attr) text)
  where
    pt_size       = font_size attr
    text_height   = numeralHeight pt_size
    text_width    = textWidth  pt_size (length text)


type TextlineRectDisplace u = (Rectangle u, GraphicF u) -> GraphicF u
    
frameWest :: Fractional u => TextlineRectDisplace u
frameWest (Rectangle w h, gf) = gf . disp (-w) (negate $ 0.5*h)

frameNorth :: Fractional u => TextlineRectDisplace u
frameNorth (Rectangle w h, gf) = gf . disp (negate $ 0.5*w) (-h)
