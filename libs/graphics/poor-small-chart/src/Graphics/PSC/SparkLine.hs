{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.SparkLine
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Spark line
--
--------------------------------------------------------------------------------

module Graphics.PSC.SparkLine
  (
  
  -- * Datatypes
    SparkLine
  , SparkLineProps(..)

  -- * Write to file
  , writeSparkLineEPS
  , writeSparkLineSVG
  
  -- * Draw
  , drawSparkLine  

  ) where

import Graphics.PSC.Utils
import Wumpus.Core

type SparkLine = DPicture
type SparkPath = DPath
type PointSize = Int


data SparkLineProps xu yu = SparkLineProps
      { point_size          :: PointSize
      , line_colour         :: DRGB
      , y_band              :: Maybe (yu,yu,DRGB)
      , x_rescale           :: xu -> Double
      , y_rescale           :: yu -> Double
      }



writeSparkLineEPS :: FilePath -> SparkLine -> IO ()
writeSparkLineEPS = writeEPS_latin1 


writeSparkLineSVG :: FilePath -> SparkLine -> IO ()
writeSparkLineSVG = writeSVG_latin1 




rescaleYCoord :: PointSize -> (u -> Double) -> (u -> Double)
rescaleYCoord pt f = rescale 0 100 0 (fromIntegral pt) . f

-- | rescale the width according to how many elements in the 
-- list of points.
--
-- Remember that a spark line is a /dataword/ in Edward Tufte\'s
-- terminology. So we take the ananlogy that each point is a 
-- letter
--
rescaleXCoord :: PointSize -> Int -> (u -> Double) -> (u -> Double)
rescaleXCoord pt_size points_count f = rescale 0 100 0 xsize . f
  where
    xsize = textWidth pt_size points_count

drawSparkLine :: SparkLineProps u v -> [(u,v)] -> SparkLine
drawSparkLine attr points = pic
  where
    pt_size     = point_size attr
    rescaleX    = rescaleXCoord pt_size (length points) (x_rescale attr)
    rescaleY    = rescaleYCoord pt_size (y_rescale attr)
    sp_path     = plotPath rescaleX rescaleY points
    sp_prim     = strokeSparkPath (line_colour attr) sp_path

    background  = case y_band attr of
                    Nothing -> Nothing
                    Just (lo,hi,rgb) -> Just $ 
                        sparkrect rgb rescaleY lo hi sp_path

    pic         = case background of
                    Nothing  -> frame  sp_prim
                    Just bkg -> frameMulti [sp_prim,bkg]
    


strokeSparkPath :: DRGB -> DPath -> DPrimitive
strokeSparkPath rgb = ostroke (rgb,line_attrs) 
  where
    line_attrs = [LineCap CapRound, LineJoin JoinRound]


sparkrect :: DRGB -> (u -> Double) -> u -> u -> SparkPath -> DPrimitive
sparkrect rgb scaleY ylo yhi sparkpath = fill rgb $ vertexPath [bl,br,ur,ul]
  where
    BBox (P2 llx _) (P2 urx _)     = boundary sparkpath 

    y0                             = scaleY ylo
    y1                             = scaleY yhi

    bl                             = P2 llx y0
    br                             = P2 urx y0
    ur                             = P2 urx y1
    ul                             = P2 llx y1
 

    


plotPath :: (u -> Double) -> (v -> Double) -> [(u,v)] -> SparkPath
plotPath xscale yscale pairs = vertexPath $ map fn pairs
  where
    fn (x,y)            = P2 (xscale x) (yscale y)
