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

import Graphics.PSC.Utils
import Wumpus.Core

import Control.Applicative


type SparkLine = DPicture
type SparkPath = DPath
type PointSize = Int


data SparkLineConfig xu yu = SparkLineConfig
      { point_size          :: PointSize
      , word_length         :: Int
      , y_band              :: Maybe (RangeBand yu)
      , x_rescale           :: xu -> Double
      , y_rescale           :: yu -> Double
      }


data SparkLineProps = SparkLineProps
      { line_width          :: Double
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

type Trace = [DPrimitive]

newtype RenderM u v a = RenderM { getRenderM :: Geom u v -> Trace -> (a,Trace) } 

instance Functor (RenderM u v) where
  fmap f (RenderM mf) = RenderM $ \e w -> let (a,w') = mf e w in (f a,w')

instance Applicative (RenderM u v) where
  pure v    = RenderM $ \_ w -> (v,w)
  mf <*> mx = RenderM $ \e w -> let (f,w')  = (getRenderM mf) e w
                                    (x,w'') = (getRenderM mx) e w'
                                in (f x,w'')
           
                                 

instance Monad (RenderM u v) where
  return a  = RenderM $ \_ w -> (a,w)
  ma >>= mf = RenderM $ \e w -> let (a,w') = getRenderM ma e w
                                in getRenderM (mf a) e w'


ask :: RenderM u v (Geom u v)
ask = RenderM $ \e w -> (e,w)

asks :: (Geom u v -> a) -> RenderM u v a
asks f = RenderM $ \e w -> (f e,w) 

tell :: DPrimitive -> RenderM u v ()
tell p = RenderM $ \_ w -> ((),p:w)

mbTell :: Maybe DPrimitive -> RenderM u v ()
mbTell mbp = RenderM $ \_ w -> ((), mbCons mbp w)
  where
    mbCons Nothing  = id
    mbCons (Just a) = (a:)



runRender :: SparkLineConfig u v -> RenderM u v a -> (a,DPicture)
runRender cfg (RenderM f) = 
    let (a,prims) = f (makeGeom cfg) [] in (a, frameMulti prims)

makeGeom :: SparkLineConfig xu yu -> Geom xu yu
makeGeom attr@(SparkLineConfig {point_size,word_length})  = 
    Geom { rect_height  = fromIntegral point_size
         , rect_width   = textWidth    point_size word_length 
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
makeRescaleX attr@(SparkLineConfig {x_rescale}) = 
    rescale 0 100 0 width . x_rescale 
  where
    width = textWidth (point_size attr) (word_length attr)

makeRescaleY :: SparkLineConfig u v -> (v -> Double)
makeRescaleY (SparkLineConfig {point_size, y_rescale}) = 
    rescale 0 100 0 (fromIntegral point_size) . y_rescale





drawSparkLine :: SparkLineConfig u v -> LineData u v -> SparkLine
drawSparkLine attr (props,points) = 
     snd $ runRender attr mkPicture
   where
     mkPicture = do { mbTell =<< mbRangeBand (y_band attr) 
                    ; sline  <- plotPath2 points
                    ; tell $ strokeSparkPath props sline
                    }


mbRangeBand :: Maybe (RangeBand v) -> RenderM u v (Maybe DPrimitive)
mbRangeBand = mbM (\(rgb,y0,y1) -> rangeBand rgb (y0,y1))


mbM :: Monad m => (a -> m b) -> Maybe a ->  m (Maybe b)
mbM _  Nothing  = return Nothing
mbM mf (Just a) = mf a >>= return . Just 



strokeSparkPath :: SparkLineProps -> SparkPath -> DPrimitive
strokeSparkPath (SparkLineProps {line_width, line_colour}) = 
    ostroke (line_colour, attrs) 
  where
    attrs = [LineWidth line_width, LineCap CapRound, LineJoin JoinRound]


rangeBand :: DRGB -> (v,v) -> RenderM u v DPrimitive
rangeBand rgb (y0,y1) = liftA2 mkBand (asks rect_width) (asks rescale_y)
  where
    mkBand w scaleY = fill rgb $ vertexPath [bl,br,ur,ul]
      where
        bl  = P2 0 (scaleY y0)
        br  = P2 w (scaleY y0)
        ur  = P2 w (scaleY y1)
        ul  = P2 0 (scaleY y1)


plotPath2 :: [(u,v)] -> RenderM u v SparkPath
plotPath2 pairs = liftA2 plot (asks rescale_x)  (asks rescale_y)
  where
    plot scaleX scaleY = vertexPath $ map (makePoint scaleX scaleY) pairs

     
makePoint :: (u -> Double) -> (v -> Double) -> (u,v) -> Point2 Double
makePoint f g (u,v) = P2 (f u) (g v)


