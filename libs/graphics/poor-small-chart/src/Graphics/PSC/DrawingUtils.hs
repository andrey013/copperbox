{-# LANGUAGE NamedFieldPuns             #-}
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

  -- * drawing
    makeStrokeProps
  , makeProjector

  , concatBackgrounds
  , straightLine


  -- * text labels
  , textlabelN
  , textlabelE
  
  ) where


import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe

--------------------------------------------------------------------------------
-- drawing

makeStrokeProps :: LineConfig -> (DRGB,[StrokeAttr])
makeStrokeProps (LineConfig rgb lw mb_dash) = 
    (rgb, catMaybes [ Just $ LineWidth lw, fmap mkDash mb_dash] )
  where
    mkDash (DashConfig offset xs) = DashPattern $ Dash offset xs


makeProjector :: Projection u -> (u -> Double)
makeProjector (Projection {proj_conv,proj_trans,proj_scale}) = 
    \u -> ((proj_conv u) - proj_trans) * proj_scale



concatBackgrounds :: (Num u, Ord u) 
                  => Picture u -> [Maybe (Picture u)] -> Picture u
concatBackgrounds top bkgrds = foldr fn top bkgrds
  where
    fn Nothing      p1 = p1
    fn (Just bkgrd) p1 = p1 `picOver` bkgrd

straightLine :: Num u => Point2 u -> Vec2 u -> Path u
straightLine pt v = path pt [lineTo $ pt .+^ v]


--------------------------------------------------------------------------------

makeTextlabel :: Fractional u 
              => (u -> u -> Vec2 u) 
              -> (DRGB,FontAttr) 
              -> String 
              -> Point2 u 
              -> Primitive u
makeTextlabel mv (rgb,font_props) text bottom_left = 
    textlabel (rgb,font_props) text (bottom_left .-^ mv text_width cap_height)
  where
    pt_size     = font_size font_props
    text_width  = textWidth  pt_size (length text)
    cap_height  = textHeight pt_size - (2 * descenderDepth pt_size)


textlabelN :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelN = makeTextlabel (\w ch -> V2 (w*0.5) ch)

textlabelE :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelE = makeTextlabel (\w ch -> V2 w (ch*0.5))