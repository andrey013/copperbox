{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Label
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Text labels 
-- (and .eps at some point?)
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Label where

import Wumpus.Core.Point
import Wumpus.Core.Polygon

import Wumpus.Drawing.Basic
import Wumpus.Drawing.GraphicsState
import Wumpus.Drawing.PostScript


-- Labels are drawn inside a clipping rectangle. This puts a burden on the 
-- user to check the output to see that the clipping rect is large enough
-- for the text it contains.


picLabel :: String -> Double -> Double -> Picture
picLabel text w h = Picture $ \pt -> 
    (clipPolygon (cliprect pt) $ drawLabel text pt, boundingBox $ cliprect pt)
  where
    cliprect pt = (rectangle w h) pt


-- labels must be drawn wrt a start point
drawLabel :: String -> DPoint2 -> WumpusM ()
drawLabel text (P2 x y) = do 
  ps_moveto x y
  ps_show text



initFont :: Font -> WumpusM ()
initFont (Font name sz) = do
  ps_findfont name
  ps_scalefont sz
  ps_setfont

setupFont :: String -> Double -> WumpusM ()
setupFont name sc = do 
   ps_findfont name
   ps_scalefont sc
   ps_setfont


