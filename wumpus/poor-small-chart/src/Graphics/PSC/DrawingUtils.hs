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


  -- * Graphic type
    wrapG
  , drawGraphic
  , cc

  -- * Drawing
  , drawingContext
  , ctxRectangleWidth
  , ctxRectangleHeight
  , ctxRectangleScaleX
  , ctxRectangleScaleY  

  , concatBackgrounds
  , straightLine
  , strokedRectangle
  , filledRectangle
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
import Wumpus.Basic.Utils.HList         -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space



wrapG :: DPrimitive -> Graphic
wrapG = wrapH 



--------------------------------------------------------------------------------
-- Composing primitives with Hughes lists


drawGraphic :: Graphic -> Maybe DPicture
drawGraphic f = step $ f []
  where
    step [] = Nothing
    step xs = Just $ frameMulti xs 


infixr 9 `cc`

cc :: ScaleCtx u v Graphic -> ScaleCtx u v Graphic -> ScaleCtx u v Graphic 
cc f g = \ctx -> f ctx . g ctx


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
 


concatBackgrounds :: Graphic -> [Graphic] -> Maybe DPicture
concatBackgrounds top bkgrds = drawGraphic $ concatH bkgrds . top


straightLine :: Stroke t => t -> DVec2 -> DPoint2 -> Graphic
straightLine t v = \pt -> wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]

rectangle :: Num u => u -> u -> Point2 u -> Path u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 


strokedRectangle :: Stroke t => t -> Double -> Double -> DPoint2 -> Graphic
strokedRectangle t w h bl = wrapG $ cstroke t $ rectangle w h bl

filledRectangle :: Fill t => t -> Double -> Double -> DPoint2 -> Graphic
filledRectangle t w h bl = wrapG $ fill t $ rectangle w h bl



strokedCircle :: DRGB -> LineWidth -> Double -> DPoint2 -> Graphic 
strokedCircle rgb lw radius = \pt -> 
    wrapG $ ellipse (rgb, LineWidth lw) radius radius pt

filledCircle :: DRGB -> Double -> DPoint2 -> Graphic
filledCircle rgb radius = \pt -> wrapG $ ellipse rgb radius radius pt 

--------------------------------------------------------------------------------

type TextLabelF = DPoint2 -> Graphic

makeTextlabel :: (Double -> Double -> Vec2 Double) 
              -> (DRGB,FontAttr) 
              -> String 
              -> TextLabelF
makeTextlabel mv (rgb,font_props) text bottom_left = 
    wrapG $ textlabel (rgb,font_props) text (bottom_left .-^ displacement)
  where
    pt_size       = font_size font_props
    text_width    = textWidth  pt_size (length text)
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

