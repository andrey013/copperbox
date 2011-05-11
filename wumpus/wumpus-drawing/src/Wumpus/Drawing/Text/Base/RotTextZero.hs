{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.RotTextZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Direction zero (left-to-right) measured text that supports 
-- radial inclination. Caveat - rendering at any inclination other 
-- than the horizontal may not look good in PostScript or SVG.
--
-- \*\* WARNING \*\* - the API for this module needs some polish.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.RotTextZero
  ( 
    LocRectTextLine
  , LocTextLine
  , TextObject

  , textline
  , bllTextline
  , blcTextline
  , ccTextline

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , rtextline
  , rescTextline

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

type LocRectTextLine u  = BoundedLocRectGraphic u
type LocTextLine u      = BoundedLocGraphic u

type TextObject u       = PosObject u



-- | Draw a single line of text.
--
textline :: (Fractional u, InterpretUnit u) 
         => String -> LocRectTextLine u
textline ss = posTextWithMargins (makeTextObject ss)


bllTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
bllTextline ss = startAddr (textline ss) BLL


blcTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
blcTextline ss = startAddr (textline ss) BLC

ccTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
ccTextline ss = startAddr (textline ss) CENTER


multiAlignLeft :: (Real u, Floating u, InterpretUnit u) 
               => String -> LocRectTextLine u
multiAlignLeft ss = 
    renderMultiLine VLeft (map makeTextObject $ lines ss)

multiAlignCenter :: (Real u, Floating u, InterpretUnit u) 
                 => String -> LocRectTextLine u
multiAlignCenter ss = 
    renderMultiLine VCenter (map makeTextObject $ lines ss)

multiAlignRight :: (Real u, Floating u, InterpretUnit u) 
                => String -> LocRectTextLine u
multiAlignRight ss = 
    renderMultiLine VRight (map makeTextObject $ lines ss)


renderMultiLine :: (Real u, Floating u, InterpretUnit u) 
                => VAlign -> [TextObject u] -> LocRectTextLine u
renderMultiLine va docs = body >>= posTextWithMargins
  where
    body  = (\dy -> alignColumnSep va dy $ reverse docs) <$> textlineSpace


makeTextObject :: InterpretUnit u => String -> TextObject u
makeTextObject = makeEscTextObject . escapeString 


makeEscTextObject :: InterpretUnit u => EscapedText -> TextObject u
makeEscTextObject esc = 
    makePosObject (textOrientationZero esc) (dcEscapedlabel esc)


-- Note inclided text will (probably) have to construct with the 
-- incline angle rather than apply it as part of the run function.
--

rtextline :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => Radian -> String -> LocRectTextLine u
rtextline ang ss = rescTextline ang (escapeString ss) 


rescTextline :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => Radian -> EscapedText -> LocRectTextLine u
rescTextline ang esc = runPosObjectR2 $ makePosObject ortt body
  where
    ortt = fmap (rotOrientation ang) $ textOrientationZero esc
    body = incline (dcREscapedlabel esc) ang



-- | Rotate an Orientation about its locus.
--
rotOrientation :: (Real u, Floating u, Ord u) 
               => Radian -> Orientation u -> Orientation u
rotOrientation ang (Orientation { or_x_minor = xmin
                                , or_x_major = xmaj
                                , or_y_minor = ymin
                                , or_y_major = ymaj }) = 
    orthoOrientation bl br tl tr  
  where
    bl  = rotateAbout ang zeroPt $ P2 (-xmin) (-ymin)
    br  = rotateAbout ang zeroPt $ P2   xmaj  (-ymin)
    tr  = rotateAbout ang zeroPt $ P2   xmaj    ymaj
    tl  = rotateAbout ang zeroPt $ P2 (-xmin)   ymaj
  

orthoOrientation :: (Num u, Ord u)
                 => Point2 u -> Point2 u -> Point2 u -> Point2 u 
                 -> Orientation u
orthoOrientation (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) = 
    Orientation { or_x_minor = abs $ min4 x0 x1 x2 x3
                , or_x_major = max4 x0 x1 x2 x3
                , or_y_minor = abs $ min4 y0 y1 y2 y3
                , or_y_major = max4 y0 y1 y2 y3
                }


min4 :: Ord u => u -> u -> u -> u -> u
min4 a b c d = min (min a b) (min c d)

max4 :: Ord u => u -> u -> u -> u -> u
max4 a b c d = max (max a b) (max c d)

