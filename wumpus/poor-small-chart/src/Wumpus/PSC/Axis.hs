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


import Wumpus.PSC.ScaleMonad

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Data.AffineSpace                 -- package: vector-space

import Control.Monad





type AxisMarkF ux u = ux -> GraphicF u




xAxis :: (Num u, Ord u, Monad m, CoordScaleM m ux uy u)
      => u -> ux -> (ux -> ux) -> RectFrameLoc u -> m [(ux,Point2 u)]
xAxis ypos ux0 next rfl = unfoldrM phi ux0
  where
    mkPt x = P2 x ypos

    phi ux = (liftM mkPt $ xScale ux) >>= \pt -> 
             if withinRectFrameLoc pt rfl 
               then return (Just ((ux,pt), next ux))
               else return Nothing

yAxis :: (Num u, Ord u, Monad m, CoordScaleM m ux uy u)
      => u -> uy -> (uy -> uy) -> RectFrameLoc u -> m [(uy,Point2 u)]
yAxis xpos uy0 next rfl = unfoldrM phi uy0
  where
    mkPt y = P2 xpos y

    phi uy = (liftM mkPt $ yScale uy) >>= \pt -> 
             if withinRectFrameLoc pt rfl 
               then return (Just ((uy,pt), next uy))
               else return Nothing



unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM mf st  = mf st >>= step
  where
    step (Just (a,st')) = liftM (a:) $ (mf st' >>= step)
    step Nothing        = return []






-- for Wumpus.Basic.Graphic...
type RectFrameLoc u = (Point2 u, RectFrame u)


withinRectFrameLoc :: (Num u, Ord u) => Point2 u -> RectFrameLoc u -> Bool
withinRectFrameLoc (P2 x y) (P2 ox oy, RectFrame w h) = 
   ox <= x && x <= (ox+w) && oy <= y && y <= (oy+h)


centeredTextline :: Fractional u => (DRGB,FontAttr) -> String -> GraphicF u
centeredTextline (rgb,attr) text ctr = 
    wrapG $ textlabel (rgb,attr) text bottom_left
  where
    pt_size       = font_size attr
    y_displace    = 0.5 * numeralHeight pt_size

    text_width    = textWidth  pt_size (length text)
    x_displace    = 0.5 * text_width

    bottom_left   = ctr .-^ vec x_displace y_displace 


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
              -> TickLabelConfig ua -> AxisMarkF ua u
makeTickLabel vec_to_end_pt disp_tick disp_lbl cfg = 
    \v -> line `cc` label v
  where
    label v    = centeredTextline (textAttrs cfg) (textF v) . disp_lbl
    line       = straightLine (lineAttrs cfg) vec_to_end_pt . disp_tick

    textF      = tick_label_text_fun cfg


tickup_textdownH :: Fractional u
                 => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickup_textdownH tick_len text_gap =  
   makeTickLabel (vvec tick_len) id (vdisp $ negate text_gap)



tickdown_textdownH :: Fractional u 
                   => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickdown_textdownH tick_len text_gap =  
    makeTickLabel (vvec $ negate tick_len) id lbl_disp 
  where
    lbl_disp = vdisp $ negate $ tick_len + text_gap


tickleftV :: Fractional u 
                   => u -> u -> TickLabelConfig ua -> AxisMarkF ua u
tickleftV tick_len text_gap =  
    makeTickLabel (hvec $ negate tick_len) id lbl_disp 
  where
    lbl_disp = hdisp $ negate $ tick_len + text_gap



axisMarks :: AxisMarkF ua u -> [(ua,Point2 u)] -> Graphic u
axisMarks fn = veloH (uncurry fn) 

