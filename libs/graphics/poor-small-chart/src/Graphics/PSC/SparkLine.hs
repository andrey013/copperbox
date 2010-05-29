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
  , SparkLineConfig(..)
  , SparkLineProps(..)
  , RangeBand

  -- * Write to file
  , writeSparkLineEPS
  , writeSparkLineSVG
  
  -- * Draw
  , drawSparkLine  

  ) where

import Graphics.PSC.Core
import Graphics.PSC.RenderMonad

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative


type SparkLine = DPicture
type SparkPath = DPath


data SparkLineConfig xu yu = SparkLineConfig
      { point_size          :: PointSize
      , word_length         :: Int
      , y_band              :: Maybe (RangeBand yu)
      , x_range             :: Range xu
      , y_range             :: (yu,yu, yu -> Double)
      }


data SparkLineProps = SparkLineProps
      { line_width          :: LineWidth
      , line_colour         :: DRGB
      }

type RangeBand yu = (DRGB, yu, yu) 

type LineData xu yu = (SparkLineProps,[(xu,yu)]) 

writeSparkLineEPS :: FilePath -> SparkLine -> IO ()
writeSparkLineEPS = writeEPS_latin1 


writeSparkLineSVG :: FilePath -> SparkLine -> IO ()
writeSparkLineSVG = writeSVG_latin1 


type SparkLineM u v a = RenderM u v a


-- | Rescale in X according the box length - calculated from
-- the /word length/.
--
-- Remember that a spark line is a /dataword/ in Edward Tufte\'s
-- terminology. So we want it to have a size similar to a word in 
-- the current font.
--


run :: SparkLineConfig u v -> SparkLineM u v a -> (a,DPicture)
run cfg@(SparkLineConfig {point_size,word_length}) mf = 
    runRender (makeGeom width height (x_range cfg) (y_range cfg)) mf
  where
    width   = textWidth    point_size word_length 
    height  = fromIntegral point_size


drawSparkLine :: SparkLineConfig u v -> LineData u v -> SparkLine
drawSparkLine attr (props,points) = 
     snd $ run attr mkPicture
   where
     mkPicture = do { mbTell =<< mbRangeBand (y_band attr) 
                    ; sline  <- plotPath points
                    ; tell $ strokeSparkPath props sline
                    }


mbRangeBand :: Maybe (RangeBand v) -> SparkLineM u v (Maybe DPrimitive)
mbRangeBand = mbM (\(rgb,y0,y1) -> rangeBand rgb (y0,y1))



strokeSparkPath :: SparkLineProps -> SparkPath -> DPrimitive
strokeSparkPath (SparkLineProps {line_width, line_colour}) = 
    ostroke (line_colour, attrs) 
  where
    attrs = [LineWidth line_width, LineCap CapRound, LineJoin JoinRound]


rangeBand :: DRGB -> (v,v) -> SparkLineM u v DPrimitive
rangeBand rgb (y0,y1) = 
    mkBand <$> (asks rect_width) <*> scaleY y0 <*> scaleY y1
  where
    mkBand w ya yb = fill rgb $ vertexPath [bl,br,ur,ul]
      where
        bl  = P2 0 ya
        br  = P2 w ya
        ur  = P2 w yb
        ul  = P2 0 yb


plotPath :: [(u,v)] -> SparkLineM u v SparkPath
plotPath coords = vertexPath <$> mapM scaleCoord coords

