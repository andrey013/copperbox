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
import Wumpus.PSC.Core
import Wumpus.PSC.ScaleRectMonad

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.Utils.HList

import Control.Applicative
import Control.Monad

-- | Orientation of the x-axis
--
data OrientX = OXTop | OXBottom
  deriving (Eq,Ord,Show)

-- | Orientation of the y-axis
--
data OrientY = OYLeft | OYRight
  deriving (Eq,Ord,Show)



type AxisMarkF ua = ua -> DGraphicF


drawXAxis :: AxisMarkF ux 
          -> ScaleRectM ux uy [(ux, DPoint2)] 
          -> ScaleRectM ux uy DGraphic
drawXAxis drawF ptGen = (veloH (uncurry drawF)) <$> ptGen 

drawYAxis :: AxisMarkF uy
          -> ScaleRectM ux uy [(uy, DPoint2)] 
          -> ScaleRectM ux uy DGraphic
drawYAxis drawF ptGen = (veloH (uncurry drawF)) <$> ptGen 


xaxisPoint :: OrientX -> ux -> ScaleRectM ux uy DPoint2
xaxisPoint ox ux = step ox <$> xScale ux <*> borderRectangle
  where
    step OXTop    x (Rectangle _ h, P2 _ y) = P2 x (y+h)
    step OXBottom x (Rectangle _ _, P2 _ y) = P2 x y

yaxisPoint :: OrientY -> uy -> ScaleRectM ux uy DPoint2
yaxisPoint oy uy = step oy <$> yScale uy <*> borderRectangle
  where
    step OYLeft  y (Rectangle _ _, P2 x _) = P2 x     y
    step OYRight y (Rectangle w _, P2 x _) = P2 (x+w) y

xaxisIxStart :: RealFrac ux => ux -> ScaleRectM ux uy ux
xaxisIxStart step = (\(x ::: _) -> ixStart (x,step)) <$> xRange

yaxisIxStart :: RealFrac uy => uy -> ScaleRectM ux uy uy
yaxisIxStart step = (\(y ::: _) -> ixStart (y,step)) <$> yRange

xaxisIxStarti :: Integral ux => ux -> ScaleRectM ux uy ux
xaxisIxStarti step = (\(x ::: _) -> ixStarti (x,step)) <$> xRange

yaxisIxStarti :: Integral uy => uy -> ScaleRectM ux uy uy
yaxisIxStarti step = (\(y ::: _) -> ixStarti (y,step)) <$> yRange


type HyloPhi    st a = st -> Maybe ((a, DPoint2),st)
type HyloPhiM m st a = st -> m (Maybe ((a, DPoint2),st))



-- Note genXPointFun is constant - might want it ouside of the 
-- loop...
--
xAxisPoints :: RealFrac ux => OrientX -> ux -> ScaleRectM ux uy [(ux,DPoint2)]
xAxisPoints ox step = xaxisIxStart step >>= unfoldrM phi
  where
    phi ux = xaxisPoint ox ux      >>= \pt ->
             withinBorderRect pt   >>= \ans ->
             if ans 
               then return (Just ((ux,pt), ux+step))
               else return Nothing

yAxisPoints :: RealFrac uy => OrientY -> uy -> ScaleRectM ux uy [(uy,DPoint2)]
yAxisPoints oy step = yaxisIxStart step >>= unfoldrM phi
  where
    phi uy = yaxisPoint oy uy       >>= \pt -> 
             withinBorderRect pt    >>= \ans ->
             if ans
               then return (Just ((uy,pt),uy+step))
               else return Nothing



-- Uck - temporary bit of copying...
--
xAxisPointsi :: Integral ux => OrientX -> ux -> ScaleRectM ux uy [(ux,DPoint2)]
xAxisPointsi ox step = xaxisIxStarti step >>= unfoldrM phi
  where
    phi ux = xaxisPoint ox ux      >>= \pt ->
             withinBorderRect pt   >>= \ans ->
             if ans 
               then return (Just ((ux,pt), ux+step))
               else return Nothing

yAxisPointsi :: Integral uy => OrientY -> uy -> ScaleRectM ux uy [(uy,DPoint2)]
yAxisPointsi oy step = yaxisIxStarti step >>= unfoldrM phi
  where
    phi uy = yaxisPoint oy uy       >>= \pt -> 
             withinBorderRect pt    >>= \ans ->
             if ans
               then return (Just ((uy,pt),uy+step))
               else return Nothing



unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM mf st  = mf st >>= step
  where
    step (Just (a,st')) = liftM (a:) $ (mf st' >>= step)
    step Nothing        = return []










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


