{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.SparkLine
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Version number
--
--------------------------------------------------------------------------------

module Graphics.PSC.SparkLine
  

  where

import Graphics.PSC.Utils
import Wumpus.Core

type SparkLine = DPicture
type SparkPath = DPath
type PointSize = Int


writeSparkLineEPS :: FilePath -> SparkLine -> IO ()
writeSparkLineEPS = writeEPS_latin1 


writeSparkLineSVG :: FilePath -> SparkLine -> IO ()
writeSparkLineSVG = writeSVG_latin1 


data SparkAttr = SparkAttr 
      { point_size          :: PointSize
      , line_colour         :: DRGB
      , horizontal_factor   :: Double
      , hrange              :: Maybe (Double,Double,DRGB)
      }

drawSparkLine :: SparkAttr -> [(Double,Double)] -> SparkLine
drawSparkLine attr points = pic
  where
    sp_path     = plotPath (point_size attr) (horizontal_factor attr) points
    sp_prim     = strokeSparkPath (line_colour attr) sp_path

    background  = case hrange attr of
                    Nothing -> Nothing
                    Just (lo,hi,rgb) -> Just $ sparkrect rgb lo hi sp_path

    pic         = case background of
                    Nothing  -> frame  sp_prim
                    Just bkg -> frameMulti [sp_prim,bkg]
    


strokeSparkPath :: DRGB -> DPath -> DPrimitive
strokeSparkPath rgb = ostroke (rgb,line_attrs) 
  where
    line_attrs = [LineCap CapRound, LineJoin JoinRound]


-- WRONG...
sparkrect :: DRGB -> Double -> Double -> SparkPath -> DPrimitive
sparkrect rgb ylo yhi sparkpath = fill rgb $ vertexPath [bl,br,ur,ul]
  where
    yloc                           = clamp 0 1 ylo
    yhic                           = clamp 0 1 yhi
    BBox (P2 llx lly) (P2 urx ury) = boundary sparkpath 

    -- this is wrong - should rescale on the (ymin,ymax)...    
    y0                             = rescale 0 1 lly ury yloc
    y1                             = rescale 0 1 lly ury yhic

    bl                             = P2 llx y0
    br                             = P2 urx y0
    ur                             = P2 urx y1
    ul                             = P2 llx y1
 

    


plotPath :: PointSize -> Double -> [(Double,Double)] -> SparkPath
plotPath pt factor pairs = vertexPath scaled_points
  where
    points              = map (uncurry P2) pairs
    (xrange,yrange)     = minMax2 points 
    scaled_points       = map (rescaleXY xrange yrange pt factor) points


rescaleXY :: XRange -> YRange -> PointSize -> Double -> DPoint2 -> DPoint2
rescaleXY (xmin,xmax) (ymin,ymax) pt stretch (P2 x y) = 
    P2 (rescale xmin xmax 0 (stretch*pt_frac) x) (rescale ymin ymax 0 pt_frac y) 
  where
    pt_frac = fromIntegral pt
   
