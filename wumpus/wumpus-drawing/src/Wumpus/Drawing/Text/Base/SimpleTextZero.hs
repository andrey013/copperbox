{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.SimpleTextZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- \"Simple\" direction zero (left-to-right) measured text. 
--
-- Plus direction zero text that supports radial inclination. 
-- Caveat - rendering at any inclination other than the horizontal 
-- may not look good in PostScript or SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.SimpleTextZero
  ( 
    LocRectTextLine
  , LocTextLine
  , PosTextLine

  , textline
  , bllTextline
  , blcTextline
  , ccTextline

  , textlineUp
  , bllTextlineUp
  , blcTextlineUp
  , ccTextlineUp

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


-- | 'LocTextLine' is a LocImage object that returns its bounding 
-- box.
--
type LocTextLine u      = LocImage u (BoundingBox u)


-- | Functional type from 'RectAddress' to LocImage.
--
-- This is the simple type for just drawing text at some
-- rectangular position (e.g. CENTER, BLL - baseline left, etc.).
--
type LocRectTextLine u  = RectAddress -> LocTextLine u


-- | Alias for 'PosObject'. 
-- 
-- 'PosTextLine' is more versatile than 'LocRectTextLine' (it 
-- supports concatentation etc.), but consequently it has a more 
-- complicated API.
-- 
type PosTextLine u       = PosObject u



-- | Build a 'LocRectTextLine' to draw a single line of text.
-- 
-- The input should contain no newline or tab characters.
--
-- The result is a @LocRect@ object which is drawn at some 
-- rectangular position (e.g. CENTER, BLL - baseline left, etc.).
--
textline :: (Fractional u, InterpretUnit u) 
         => String -> LocRectTextLine u
textline ss = posTextWithMargins (makePosTextLine ss)


-- | Draw a single line of text, start point is baseline left.
--
bllTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
bllTextline ss = textline ss BLL


-- | Draw a single line of text, startpoint is baseline center.
--
blcTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
blcTextline ss = textline ss BLC


-- | Draw a single line of text, start point is center-center.
--
ccTextline :: (Floating u, InterpretUnit u) 
            => String -> LocTextLine u
ccTextline ss = textline ss CENTER


-- | /Upright/ version of 'textline'.
--
-- The vertical orientation is calculated using only the 
-- cap-height and ignoring the descender depth.
--
-- In circumstances where you want visually good vertical 
-- centering and know the input string has no descenders 
-- (e.g. digits) use this rather than 'textline'.
--
textlineUp :: (Fractional u, InterpretUnit u) 
           => String -> LocRectTextLine u
textlineUp ss = posTextWithMargins (makePosTextLineUp ss)


-- | /Upright/ version of 'textline'.
--
bllTextlineUp :: (Floating u, InterpretUnit u) 
              => String -> LocTextLine u
bllTextlineUp ss = textlineUp ss BLL


-- | /Upright/ version of 'textline'.
--
blcTextlineUp :: (Floating u, InterpretUnit u) 
              => String -> LocTextLine u
blcTextlineUp ss = textlineUp ss BLC

-- | /Upright/ version of 'textline'.
--
ccTextlineUp :: (Floating u, InterpretUnit u) 
             => String -> LocTextLine u
ccTextlineUp ss = textlineUp ss CENTER


-- | Multiline text aligned to the left. 
--
-- The input string is split with the Prelude function 'lines'.
-- 
multiAlignLeft :: (Real u, Floating u, InterpretUnit u) 
               => String -> LocRectTextLine u
multiAlignLeft ss = 
    renderMultiLine VALIGN_LEFT (map makePosTextLine $ lines ss)


-- | Multiline text aligned to the center. 
--
-- The input string is split with the Prelude function 'lines'.
-- 
multiAlignCenter :: (Real u, Floating u, InterpretUnit u) 
                 => String -> LocRectTextLine u
multiAlignCenter ss = 
    renderMultiLine VALIGN_CENTER (map makePosTextLine $ lines ss)


-- | Multiline text aligned to the right. 
--
-- The input string is split with the Prelude function 'lines'.
-- 
multiAlignRight :: (Real u, Floating u, InterpretUnit u) 
                => String -> LocRectTextLine u
multiAlignRight ss = 
    renderMultiLine VALIGN_RIGHT (map makePosTextLine $ lines ss)

-- | Render multiple lines...
--
renderMultiLine :: (Real u, Floating u, InterpretUnit u) 
                => VAlign -> [PosTextLine u] -> LocRectTextLine u
renderMultiLine va docs = \raddr -> 
    body >>= \ans -> posTextWithMargins ans raddr
  where
    body  = (\dy -> alignColumnSep va dy docs) <$> textlineSpace


-- | Helper.
--
makePosTextLine :: InterpretUnit u 
                => String -> PosTextLine u
makePosTextLine = 
    makeEscPosTextLine CAP_HEIGHT_PLUS_DESCENDER . escapeString 


-- | Upright version of 'makePosTextLine'.
--
makePosTextLineUp :: InterpretUnit u 
                => String -> PosTextLine u
makePosTextLineUp = 
    makeEscPosTextLine JUST_CAP_HEIGHT . escapeString 


makeEscPosTextLine :: InterpretUnit u 
                   =>  TextVSize -> EscapedText -> PosTextLine u
makeEscPosTextLine vsz esc = 
    makePosObject (textOrientationZero vsz esc) (dcEscapedlabel esc)


--------------------------------------------------------------------------------
-- Rotated - inclined text


-- Note inclided text will (probably) have to construct with the 
-- incline angle rather than apply it as part of the run function.
--

-- | Incline version of 'textline'.
--
rtextline :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => Radian -> String -> LocRectTextLine u
rtextline ang ss = rescTextline ang (escapeString ss) 


-- | Inclined version of 'escTextline'.
--
rescTextline :: (Real u, Floating u, Ord u, InterpretUnit u) 
             => Radian -> EscapedText -> LocRectTextLine u
rescTextline ang esc = makeRotEscText CAP_HEIGHT_PLUS_DESCENDER ang esc


makeRotEscText :: (Real u, Floating u, Ord u, InterpretUnit u) 
               => TextVSize -> Radian -> EscapedText -> LocRectTextLine u
makeRotEscText vsz ang esc = \raddr -> runPosObject raddr $ makePosObject ortt body
  where
    ortt = fmap (rotOrientation ang) $ textOrientationZero vsz esc
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

