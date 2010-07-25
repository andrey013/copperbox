{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Interpret
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Interpret
  (

    interpret

  ) where


import Wumpus.Timing.Alphabet
import Wumpus.Timing.Drawing
import Wumpus.Timing.Width

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.SVGColours

interpret :: [Letter] -> DGraphic
interpret []     = blankG
interpret (l:ls) = let (next,x2,g1) = interp l Init 0 in g1 . rest next x2 ls
  
pt :: Width -> DPoint2
pt w = P2 (fromIntegral $ 10*halfcount w) 0

std_props :: Props
std_props = defaultProps

zprops :: Props
zprops = std_props { stroke_colour = blue }

xprops :: Props
xprops = std_props { stroke_colour = red }

interp :: Letter -> Prefix -> Width -> (Prefix,Width,DGraphic)
interp (H n) pre w = (FromTop, w+n, lineHigh pre (halfcount n) 10 std_props (pt w))
interp (L n) pre w = (FromBtm, w+n, lineLow  pre (halfcount n) 10 std_props (pt w))
interp (Z n) pre w = (FromCtr, w+n, lineMid  pre (halfcount n) 10 zprops (pt w))
interp (X n) pre w = (FromCtr, w+n, lineMid  pre (halfcount n) 10 xprops (pt w))

interp G     pre w = (pre, w, glitch 10 std_props (pt w)) -- props wrong...

rest :: Prefix -> Width -> [Letter] -> DGraphic
rest _   _ [] = blankG
rest pre w (x:xs) = let (next,w2,g1) = interp x pre w in g1 . rest next w2 xs
