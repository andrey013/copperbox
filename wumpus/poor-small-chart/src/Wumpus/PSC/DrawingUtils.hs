{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawing utilities atop wumpus-core
--
--------------------------------------------------------------------------------

module Graphics.PSC.DrawingUtils
  (



  -- * Drawing
    drawingContext
  , ctxRectangleWidth
  , ctxRectangleHeight
  , ctxRectangleScaleX
  , ctxRectangleScaleY  

  , concatBackgrounds
  , strokedCircle
  , filledCircle

  -- * Text labels
  , TextLabelF
  , textlabelU
  , textlabelN
  , textlabelS
  , textlabelE
  , textlabelW
  , textlabelC
  

  ) where


import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList


import Data.AffineSpace                 -- package: vector-space






--------------------------------------------------------------------------------
-- Drawing


drawingContext :: (Num u, Num v) 
               => Range u -> (u -> Double) 
               -> Range v -> (v -> Double) 
               -> DrawingRectangle
               -> DrawingContext u v
drawingContext (u0:::u1) fromU (v0:::v1) fromV (w,h) = ((w,h), scaleX, scaleY) 
  where
    scaleX = rescale (fromU u0) (fromU u1) 0 w . fromU
    scaleY = rescale (fromV v0) (fromV v1) 0 h . fromV


ctxRectangleWidth  :: ScaleCtx u v Double
ctxRectangleWidth  = \((w,_),_,_) -> w

ctxRectangleHeight :: ScaleCtx u v Double
ctxRectangleHeight = \((_,h),_,_) -> h

ctxRectangleScaleX :: ScaleCtx u v (u -> Double)
ctxRectangleScaleX = \(_,fX,_) -> fX

ctxRectangleScaleY :: ScaleCtx u v (v -> Double)
ctxRectangleScaleY = \(_,_,fY) -> fY
 


concatBackgrounds :: DGraphic -> [DGraphic] -> Maybe DPicture
concatBackgrounds top bkgrds = drawGraphic $ concatH bkgrds . top



{-
strokedCircle :: DRGB -> LineWidth -> Double -> DPoint2 -> DGraphic 
strokedCircle rgb lw radius = \pt -> 
    wrapG $ ellipse (rgb, LineWidth lw) radius radius pt

filledCircle :: DRGB -> Double -> DPoint2 -> DGraphic
filledCircle rgb radius = \pt -> wrapG $ ellipse rgb radius radius pt 
-}

--------------------------------------------------------------------------------

type TextLabelF = DPoint2 -> DGraphic

makeTextlabel :: (Double -> Double -> Vec2 Double) 
              -> (DRGB,FontAttr) 
              -> String 
              -> TextLabelF
makeTextlabel mv (rgb,font_props) msg bottom_left = 
    wrapG $ textlabel (rgb,font_props) msg (bottom_left .-^ displacement)
  where
    pt_size       = font_size font_props
    text_width    = textWidth  pt_size (length msg)
    cap_height    = textHeight pt_size - (2 * descenderDepth pt_size)
    displacement  = mv text_width cap_height

textlabelU :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelU (rgb,font_props) = 
    makeTextlabel (\ _ _ -> V2 0 (negate dd)) (rgb,font_props)
  where
    pt_size     = font_size font_props
    dd          = descenderDepth pt_size


textlabelN :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelN = makeTextlabel (\w cap_height -> V2 (w*0.5) cap_height)



textlabelS :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelS = makeTextlabel (\w _ -> V2 (w*0.5) 0)


textlabelE :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelE = makeTextlabel (\w cap_height -> V2 w (cap_height*0.5))

textlabelW :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelW = makeTextlabel (\_ cap_height -> V2 0 (cap_height*0.5))

textlabelC :: (DRGB,FontAttr) -> String -> TextLabelF
textlabelC = makeTextlabel (\w cap_height -> V2 (w*0.5) (cap_height*0.5))



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- WUMPUS_CORE additions

