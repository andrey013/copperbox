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

data Geom xu yu = Geom 
      { rect_height         :: Double
      , rect_width          :: Double
      , rescale_x           :: xu -> Double
      , rescale_y           :: yu -> Double
      }


type RangeBand yu = (DRGB, yu, yu) 

type LineData xu yu = (SparkLineProps,[(xu,yu)]) 

writeSparkLineEPS :: FilePath -> SparkLine -> IO ()
writeSparkLineEPS = writeEPS_latin1 


writeSparkLineSVG :: FilePath -> SparkLine -> IO ()
writeSparkLineSVG = writeSVG_latin1 


type SparkLineM u v a = RenderM (Geom u v) a


run :: SparkLineConfig u v -> SparkLineM u v a -> (a,DPicture)
run cfg mf = runRender (makeGeom cfg) mf


makeGeom :: SparkLineConfig xu yu -> Geom xu yu
makeGeom attr@(SparkLineConfig {point_size,word_length})  = 
    Geom { rect_width   = textWidth    point_size word_length 
         , rect_height  = fromIntegral point_size
         , rescale_x    = makeRescaleX attr
         , rescale_y    = makeRescaleY attr
         }



-- | Rescale in X according the box length - calculated from
-- the /word length/.
--
-- Remember that a spark line is a /dataword/ in Edward Tufte\'s
-- terminology. So we want it to have a size similar to a word in 
-- the current font.
--
makeRescaleX :: SparkLineConfig u v -> (u -> Double)
makeRescaleX attr@(SparkLineConfig {x_range = (x0,x1,fn)}) = 
    rescale (fn x0) (fn x1) 0 width . fn
  where
    width = textWidth (point_size attr) (word_length attr)

makeRescaleY :: SparkLineConfig u v -> (v -> Double)
makeRescaleY (SparkLineConfig {point_size, y_range = (y0,y1,fn)}) = 
    rescale (fn y0) (fn y1) 0 (fromIntegral point_size) . fn



drawSparkLine :: SparkLineConfig u v -> LineData u v -> SparkLine
drawSparkLine attr (props,points) = 
     snd $ run attr mkPicture
   where
     mkPicture = do { mbTell =<< mbRangeBand (y_band attr) 
                    ; sline  <- plotPath2 points
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
rangeBand rgb (y0,y1) = liftA2 mkBand (asks rect_width) (asks rescale_y)
  where
    mkBand w scaleY = fill rgb $ vertexPath [bl,br,ur,ul]
      where
        bl  = P2 0 (scaleY y0)
        br  = P2 w (scaleY y0)
        ur  = P2 w (scaleY y1)
        ul  = P2 0 (scaleY y1)


plotPath2 :: [(u,v)] -> SparkLineM u v SparkPath
plotPath2 pairs = liftA2 plot (asks rescale_x)  (asks rescale_y)
  where
    plot scaleX scaleY = vertexPath $ map (makePoint scaleX scaleY) pairs

     
makePoint :: (u -> Double) -> (v -> Double) -> (u,v) -> Point2 Double
makePoint f g (u,v) = P2 (f u) (g v)


