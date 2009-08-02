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

import Wumpus.Core.BoundingBox
import Wumpus.Core.Frame
import Wumpus.Core.Point
import Wumpus.Core.Polygon
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript

import Data.AffineSpace

import Control.Monad ( zipWithM_ )

-- Labels are drawn inside a clipping rectangle. This puts a burden on the 
-- user to check the output to see that the clipping rect is large enough
-- for the text it contains.
-- 
-- One option would be to add support for em/ex for some of the standard
-- PostSript fonts (Helvetica, Times-Roman). em is the approx width of 
-- 'M' in the current font, ex is the approx height of 'x'. 
-- It would not be too burdensome to measure a few fonts...


-- Note - Labels use a clipping path so they must be bracketed with  
-- saveExecRestore. 

picLabel :: String -> Double -> Double -> Picture
picLabel text w h = Picture $ \frm -> (mf frm, getBoundingBox $ cliprect frm)
  where
    mf frm = saveExecRestore id $ clipPolygon (cliprect frm) 
                                $ drawLabel text (origin frm)
    cliprect frm = (rectangle w h) (origin frm)


-- multi-line text is generally achieved in PostScript with moveto
-- acting as a carriage return.

-- currently this draws from bottom to top (hence the reverse of the list),
-- it needs improving...
picText :: String -> Double -> Double -> Double -> Picture
picText text w h line_height = Picture $ \frm -> (mf frm, getBoundingBox $ cliprect frm)
  where
    
    mf frm = saveExecRestore id $ clipPolygon (cliprect frm) 
                                $ zipWithM_ drawLabel (reverse ls) (points $ origin frm)
    cliprect frm = (rectangle w h) (origin frm)
    
    ls = lines text
    points pt = take (length ls) $ iterate (.+^ (V2 0 (line_height))) pt

    

-- labels must be drawn wrt a start point
drawLabel :: String -> DPoint2 -> WumpusM ()
drawLabel text (P2 x y) = do 
  ps_moveto x y
  ps_show text




