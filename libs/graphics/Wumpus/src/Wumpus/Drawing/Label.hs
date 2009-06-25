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
import Wumpus.Core.PostScript


setupFont :: String -> Double -> WumpusM ()
setupFont name sc = do 
   ps_findfont name
   ps_scalefont sc
   ps_setfont

-- labels must be drawn wrt a start point
drawText :: DPoint2 -> String -> WumpusM ()
drawText (P2 x y) text = do 
  ps_moveto x y
  ps_show text