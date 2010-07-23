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
  , pointHylo

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



--------------------------------------------------------------------------------



hylor :: (st -> Maybe (a,st)) -> (a -> b -> b) -> b -> st -> b
hylor f g b0 st0 = step b0 (f st0)
  where
    step acc Nothing        = acc
    step acc (Just (a,st))  = step (g a acc) (f st) 


-- Simple type restricted hylomorphism - generates points, 
-- amalgamates to a Graphic.
--
pointHylo :: (st -> Maybe (Point2 u, st)) 
          -> (Point2 u -> Graphic u) 
          -> st 
          -> Graphic u
pointHylo f g = hylor f (\pt acc -> acc . g pt) blankG


-- This would be unnecessary on a real hylomorphism as the 
-- result type of the unfold step could pair the state with
-- the answer.
--
  
pointHyloSt :: (st -> Maybe (Point2 u, st)) 
            -> (st -> Point2 u -> Graphic u) 
            -> st 
            -> Graphic u
pointHyloSt g f st0 = step blankG (g st0) 
  where
    step acc Nothing        = acc
    step acc (Just (pt,st)) = step (acc . f st pt) (g st)