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

import Wumpus.Drawing.PostScript


-- Labels are always oriented in 2-space
data Label a  = Label (Point2 a) String
  deriving (Eq,Show)

type CoLabel a = Point2 a -> Label a

type DLabel = Label Double
type DCoLabel = CoLabel Double


label :: String -> CoLabel a
label text = \o -> Label o text

setupFont :: String -> Double -> WumpusM ()
setupFont name sc = do 
   ps_findfont name
   ps_scalefont sc
   ps_setfont

-- labels must be drawn wrt a start point
drawLabel :: DLabel -> WumpusM ()
drawLabel (Label (P2 x y) text) = do 
  ps_moveto x y
  ps_show text


