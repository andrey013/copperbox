{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Axis
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Axes / grids
--
--------------------------------------------------------------------------------

module Wumpus.PSC.Axis
  where


import Wumpus.PSC.BasicAdditions
import Wumpus.PSC.Bivariate
import Wumpus.PSC.BivariateGraphic
import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic

-- | Orientation of the x-axis
--
data OrientX = OXTop | OXBottom
  deriving (Eq,Ord,Show)

-- | Orientation of the y-axis
--
data OrientY = OYLeft | OYRight
  deriving (Eq,Ord,Show)

type AxisMarkF ua = ua -> DGraphicF



xaxisPoint :: OrientX -> ux -> Bivariate ux uy -> DPoint2
xaxisPoint ox ux bv = step ox (scaleX ux bv) (borderRectangle bv)
  where
    step OXTop    x (Rectangle _ h, P2 _ y) = P2 x (y+h)
    step OXBottom x (Rectangle _ _, P2 _ y) = P2 x y


yaxisPoint :: OrientY -> uy -> Bivariate ux uy -> DPoint2
yaxisPoint oy uy bv = step oy (scaleY uy bv) (borderRectangle bv)
  where
    step OYLeft  y (Rectangle _ _, P2 x _) = P2 x     y
    step OYRight y (Rectangle w _, P2 x _) = P2 (x+w) y



type Hylo2Phi   st a = st -> Maybe ((a, DPoint2),st)


xAxisPhi :: Num ux 
         => OrientX -> ux -> Bivariate ux uy -> Hylo2Phi ux ux 
xAxisPhi ox step bv = \ux -> let pt = xaxisPoint ox ux bv in 
    if withinBorderRect pt bv then (Just ((ux,pt), ux+step))
                              else Nothing


yAxisPhi :: Num uy
         => OrientY -> uy -> Bivariate ux uy -> Hylo2Phi uy uy 
yAxisPhi oy step bv = \uy -> let pt = yaxisPoint oy uy bv in 
    if withinBorderRect pt bv then (Just ((uy,pt), uy+step))
                              else Nothing



xaxisIxStart :: RealFrac ux => ux -> Bivariate ux uy -> ux
xaxisIxStart step bv = let (x ::: _) = xRange bv in ixStart (x,step)

yaxisIxStart :: RealFrac uy => uy -> Bivariate ux uy -> uy
yaxisIxStart step bv = let (y ::: _) = yRange bv in ixStart (y,step)

xaxisIxStarti :: Integral ux => ux -> Bivariate ux uy -> ux
xaxisIxStarti step bv = let (x ::: _) = xRange bv in ixStarti (x,step)

yaxisIxStarti :: Integral uy => uy -> Bivariate ux uy -> uy
yaxisIxStarti step bv = let (y ::: _) = yRange bv in ixStarti (y,step)


xAxis :: RealFrac ux 
      => OrientX -> ux -> (ux -> DGraphicF) -> BivariateGraphic ux uy
xAxis orX step drawF bv = pointHylo2 phi drawF (xaxisIxStart step bv) 
  where
    phi = xAxisPhi orX step bv

yAxis :: RealFrac uy
      => OrientY -> uy -> (uy -> DGraphicF) -> BivariateGraphic ux uy
yAxis orY step drawF bv = pointHylo2 phi drawF (yaxisIxStart step bv) 
  where
    phi = yAxisPhi orY step bv


xAxisi :: Integral ux 
       => OrientX -> ux -> (ux -> DGraphicF) -> BivariateGraphic ux uy
xAxisi orX step drawF bv = pointHylo2 phi drawF (xaxisIxStarti step bv) 
  where
    phi = xAxisPhi orX step bv

yAxisi :: Integral uy
       => OrientY -> uy -> (uy -> DGraphicF) -> BivariateGraphic ux uy
yAxisi orY step drawF bv = pointHylo2 phi drawF (yaxisIxStarti step bv) 
  where
    phi = yAxisPhi orY step bv







--------------------------------------------------------------------------------

type TickDraw  t u = (t, Point2T u, Vec2 u)
type LabelDraw   u = (DRGB, FontAttr, Point2T u)

data TickLabelConfig ua = TickLabelConfig 
      { tick_label_font_colour    :: DRGB
      , tick_label_font_attr      :: FontAttr
      , tick_label_text_fun       :: ua -> String
      , tick_label_line_colour    :: DRGB
      , tick_label_line_width     :: Double
      }

tickLabelConfig :: DRGB -> FontAttr -> (ua -> String) -> TickLabelConfig ua
tickLabelConfig rgb font_attr textF = 
    TickLabelConfig { tick_label_font_colour    = rgb
                    , tick_label_font_attr      = font_attr
                    , tick_label_text_fun       = textF
                    , tick_label_line_colour    = rgb
                    , tick_label_line_width     = 1.0
                    }

lineAttrs :: TickLabelConfig ua -> (DRGB, StrokeAttr)
lineAttrs (TickLabelConfig 
            { tick_label_line_colour = rgb
            , tick_label_line_width  = lw} ) = (rgb, LineWidth lw)

textAttrs :: TickLabelConfig ua -> (DRGB, FontAttr)
textAttrs (TickLabelConfig 
            { tick_label_font_colour = rgb
            , tick_label_font_attr   = attr} ) = (rgb,attr)


makeTickLabel :: DVec2 -> Point2T Double -> Point2T Double
              -> TextlineRectDisplace Double
              -> TickLabelConfig ua 
              -> AxisMarkF ua
makeTickLabel vec_to_end_pt disp_tick disp_lbl move_text cfg = 
    \v -> (positionWith disp_tick line) `cc` (positionWith disp_lbl $ label v)
  where
    label v    = move_text $ textlineRect (textAttrs cfg) (textF v)
    line       = straightLine (lineAttrs cfg) vec_to_end_pt

    textF      = tick_label_text_fun cfg



tickup_textdownH :: Double -> Double -> TickLabelConfig ua -> AxisMarkF ua
tickup_textdownH tick_len text_gap =  
   makeTickLabel (vvec tick_len) id (vdisp $ negate text_gap) frameNorth



tickdown_textdownH :: Double -> Double -> TickLabelConfig ua -> AxisMarkF ua
tickdown_textdownH tick_len text_gap =  
    makeTickLabel (vvec $ negate tick_len) id lbl_disp frameNorth
  where
    lbl_disp = vdisp $ negate $ tick_len + text_gap


tickleftV :: Double -> Double -> TickLabelConfig ua -> AxisMarkF ua
tickleftV tick_len text_gap =  
    makeTickLabel (hvec $ negate tick_len) id lbl_disp frameWest
  where
    lbl_disp = hdisp $ negate $ tick_len + text_gap


