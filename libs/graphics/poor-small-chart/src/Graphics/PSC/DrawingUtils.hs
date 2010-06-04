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
  -- Composing primitives
    HPrim
  , drawHPrim

  -- * drawing
  , makeStrokeProps
  , makeProjector

  , concatBackgrounds
  , straightLine
  , rectPoints


  -- * text labels
  , textlabelU
  , textlabelN
  , textlabelS
  , textlabelE
  , textlabelW
  
  -- wumpus-core additions
  , capHeight


  ) where


import Graphics.PSC.Core
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe


--------------------------------------------------------------------------------
-- Composing primitives with Hughes lists

type HPrim u = H (Primitive u)

drawHPrim :: (Floating u, Ord u) => HPrim u -> Maybe (Picture u)
drawHPrim f = step $ f []
  where
    step [] = Nothing
    step xs = Just $ frameMulti xs 

--------------------------------------------------------------------------------
-- Drawing

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


rectPoints :: Num u => u -> u -> Point2 u -> [Point2 u]
rectPoints w h bl = [ bl, br, tr, tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 

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

textlabelU :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelU (rgb,font_props) text bottom_left = 
    textlabel (rgb,font_props) text (bottom_left .+^ vvec dd)
  where
    pt_size     = font_size font_props
    dd          = descenderDepth pt_size


textlabelN :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelN = makeTextlabel (\w cap_height -> V2 (w*0.5) cap_height)



textlabelS :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelS = makeTextlabel (\w _ -> V2 (w*0.5) 0)


textlabelE :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelE = makeTextlabel (\w cap_height -> V2 w (cap_height*0.5))

textlabelW :: Fractional u 
           => (DRGB,FontAttr) -> String -> Point2 u -> Primitive u
textlabelW = makeTextlabel (\_ cap_height -> V2 0 (cap_height*0.5))


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- WUMPUS_CORE additions


-- The height of an upper case letter (without ascender or 
-- descender).
--
capHeight :: Fractional u => FontSize -> u
capHeight sz = textHeight sz - (2 * descenderDepth sz)