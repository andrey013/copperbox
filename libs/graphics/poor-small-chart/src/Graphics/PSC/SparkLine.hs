{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.SparkLine
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC - NamedFieldPuns
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
      , word_length         :: Int
      , line_colour         :: DRGB
      , y_band              :: Maybe (yu,yu,DRGB)
      , x_rescale           :: xu -> Double
      , y_rescale           :: yu -> Double
      }



writeSparkLineEPS :: FilePath -> SparkLine -> IO ()
writeSparkLineEPS = writeEPS_latin1 


writeSparkLineSVG :: FilePath -> SparkLine -> IO ()
writeSparkLineSVG = writeSVG_latin1 




boxLength :: SparkLineProps u v -> Double
boxLength (SparkLineProps {point_size,word_length}) = 
    textWidth point_size word_length

-- | Rescale in X according the box length - calculated from
-- the /word length/.
--
-- Remember that a spark line is a /dataword/ in Edward Tufte\'s
-- terminology. So we want it to have a size similar to a word in 
-- the current font.
--
makeRescaleX :: SparkLineProps u v -> (u -> Double)
makeRescaleX props@(SparkLineProps {x_rescale}) = 
    rescale 0 100 0 (boxLength props) . x_rescale 
 

makeRescaleY :: SparkLineProps u v -> (v -> Double)
makeRescaleY (SparkLineProps {point_size, y_rescale}) = 
    rescale 0 100 0 (fromIntegral point_size) . y_rescale



drawSparkLine :: SparkLineProps u v -> [(u,v)] -> SparkLine
drawSparkLine attr@(SparkLineProps {line_colour,y_band}) points = pic
  where
    rescaleX    = makeRescaleX attr
    rescaleY    = makeRescaleY attr 
    sp_path     = plotPath rescaleX rescaleY points
    sp_prim     = strokeSparkPath line_colour sp_path

    background  = case y_band of
                    Nothing -> Nothing
                    Just (lo,hi,rgb) -> Just $ 
                        sparkrect rgb (boxLength attr) rescaleY lo hi

    pic         = case background of
                    Nothing  -> frame  sp_prim
                    Just bkg -> frameMulti [sp_prim,bkg]
    


strokeSparkPath :: DRGB -> DPath -> DPrimitive
strokeSparkPath rgb = ostroke (rgb,line_attrs) 
  where
    line_attrs = [LineCap CapRound, LineJoin JoinRound]


sparkrect :: DRGB -> Double -> (u -> Double) -> u -> u -> DPrimitive
sparkrect rgb box_width scaleY ylo yhi = fill rgb $ vertexPath [bl,br,ur,ul]
  where
    y0                             = scaleY ylo
    y1                             = scaleY yhi

    bl                             = P2 0         y0
    br                             = P2 box_width y0
    ur                             = P2 box_width y1
    ul                             = P2 0         y1
 

plotPath :: (u -> Double) -> (v -> Double) -> [(u,v)] -> SparkPath
plotPath xscale yscale pairs = vertexPath $ map fn pairs
  where
    fn (x,y)            = P2 (xscale x) (yscale y)
