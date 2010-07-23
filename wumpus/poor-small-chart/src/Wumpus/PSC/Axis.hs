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

genXPointFun  :: OrientX -> ScaleRectM ux uy (Double -> Point2 Double)
genXPointFun o = step o <$> borderRectangle
  where
    step OXTop    (Rectangle _ h, P2 _ y) = (\u -> P2 u (y+h))
    step OXBottom (Rectangle _ _, P2 _ y) = (\u -> P2 u y)


genYPointFun  :: OrientY -> ScaleRectM ux uy (Double -> Point2 Double)
genYPointFun o = step o <$> borderRectangle
  where
    step OYLeft  (Rectangle _ _, P2 x _) = (\u -> P2 x u)
    step OYRight (Rectangle w _, P2 x _) = (\u -> P2 (x+w) u)


-- Note genXPointFun is constant - might want it ouside of the 
-- loop...
--
xAxisPoints :: OrientX -> ux -> (ux -> ux) -> ScaleRectM ux uy [(ux,DPoint2)]
xAxisPoints ox ux0 next = unfoldrM phi ux0
  where
    phi ux = (genXPointFun ox <*> xScale ux) >>= \pt ->
             withinBorderRect pt             >>= \ans ->
             if ans 
               then return (Just ((ux,pt), next ux))
               else return Nothing

yAxisPoints :: OrientY -> uy -> (uy -> uy) -> ScaleRectM ux uy [(uy,DPoint2)]
yAxisPoints oy uy0 next = unfoldrM phi uy0
  where
    phi uy = (genYPointFun oy <*> yScale uy)  >>= \pt -> 
             withinBorderRect pt              >>= \ans ->
             if ans
               then return (Just ((uy,pt), next uy))
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


