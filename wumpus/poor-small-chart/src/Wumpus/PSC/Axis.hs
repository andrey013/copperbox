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


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.Utils.HList

import Control.Monad





type AxisMarkF ux u = ux -> GraphicF u




xAxis :: (Num u, Ord u, Monad m, CoordScaleM m ux uy u)
      => u -> ux -> (ux -> ux) -> RectangleLoc u -> m [(ux,Point2 u)]
xAxis ypos ux0 next rfl = unfoldrM phi ux0
  where
    mkPt x = P2 x ypos

    phi ux = (liftM mkPt $ xScale ux) >>= \pt -> 
             if withinRectangleLoc pt rfl 
               then return (Just ((ux,pt), next ux))
               else return Nothing

yAxis :: (Num u, Ord u, Monad m, CoordScaleM m ux uy u)
      => u -> uy -> (uy -> uy) -> RectangleLoc u -> m [(uy,Point2 u)]
yAxis xpos uy0 next rfl = unfoldrM phi uy0
  where
    mkPt y = P2 xpos y

    phi uy = (liftM mkPt $ yScale uy) >>= \pt -> 
             if withinRectangleLoc pt rfl 
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


makeTickLabel :: Fractional u
              => Vec2 u -> Point2T u -> Point2T u 
              -> TextlineRectDisplace u
              -> TickLabelConfig ua 
              -> AxisMarkF ua u
makeTickLabel vec_to_end_pt disp_tick disp_lbl move_text cfg = 
    \v -> (positionWith disp_tick line) `cc` (positionWith disp_lbl $ label v)
  where
    label v    = move_text $ textlineRect (textAttrs cfg) (textF v)
    line       = straightLine (lineAttrs cfg) vec_to_end_pt

    textF      = tick_label_text_fun cfg



tickup_textdownH :: Fractional u
                 => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickup_textdownH tick_len text_gap =  
   makeTickLabel (vvec tick_len) id (vdisp $ negate text_gap) frameNorth



tickdown_textdownH :: Fractional u 
                   => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickdown_textdownH tick_len text_gap =  
    makeTickLabel (vvec $ negate tick_len) id lbl_disp frameNorth
  where
    lbl_disp = vdisp $ negate $ tick_len + text_gap


tickleftV :: Fractional u 
                   => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickleftV tick_len text_gap =  
    makeTickLabel (hvec $ negate tick_len) id lbl_disp frameWest
  where
    lbl_disp = hdisp $ negate $ tick_len + text_gap



axisMarks :: AxisMarkF ua u -> [(ua,Point2 u)] -> Graphic u
axisMarks fn = veloH (uncurry fn) 

