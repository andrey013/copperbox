{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.SparkLine
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Spark lines
--
--------------------------------------------------------------------------------

module Wumpus.PSC.SparkLine
  where


import Wumpus.PSC.Bivariate
import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic


import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid




-- A sparkline is obviously a graphic. 
--
-- Edward Tufte posited sparklines as inter-word graphics within 
-- text, so they must be moveable and hence are a LocGraphic.
-- 
-- Data is plotted relative to the initial point. As the initial 
-- point is parametric, data has to be relative to the initial 
-- point rather than as absolute positions so it is represented
-- as a list of vector displacements from the original point.
-- 

type BivLocGraphic ux uy u = Bivariate ux uy u (LocGraphic u)

type SparkLineF u = [Vec2 u] -> LocGraphic u

-- Note - this is actually a very complicated type that 
-- interprets the dataset as it draws it. Potentially splitting 
-- the interpretation from the drawing might simplify things.
-- 
type SparkLine ux uy u = Bivariate ux uy u (LocGraphic u)

simpleLine :: Num u => SparkLineF u
simpleLine vs pt = openStroke $ vertexPath $ map (\v -> pt .+^ v) vs 



-- univariateChain :: (Fractional ux, Num uy) => [uy] -> BivariateAlg ux uy 
-- univariateChain ys = univariateY ys 

-- sparkChain :: Dataset ux uy 


sparkLine :: SparkLineF u -> Dataset ux uy -> Bivariate ux uy u (LocGraphic u)
sparkLine drawF ds =  mapM (uncurry scaleVec) ds >>= \vs -> return (drawF vs)

{-
sparklineRectangle :: FontAttr -> Int -> DPoint2 -> DRectangle
sparklineRectangle attr letter_count pt = 
    Rectangle pt (fromPtSize $ textWidth sz letter_count) (fromIntegral sz) 
  where
    sz = font_size attr
-}


--------------------------------------------------------------------------------
-- Range band




rangeBand :: (Num ux, Num uy, Fractional u) 
          => uy -> uy -> Bivariate ux uy u (LocGraphic u)
rangeBand uy0 uy1 =  
    (\(w,_) y0 y1 -> filledRectangle w (y1 -y0) . vdisplace y0) 
      <$> rangeDimensions <*> scaleY uy0 <*> scaleY uy1


-- NOTE - having empty graphics seems useful even if printing 
-- them is a problem...
--
noRangeBand :: Bivariate ux uy u (LocGraphic u)
noRangeBand = return mempty




--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



