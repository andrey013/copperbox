{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic.Drawing
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Graphic
import Wumpus.Basic.Graphic.Image
import Wumpus.Basic.Paths
import Wumpus.Basic.Paths.Base
import Wumpus.Basic.Paths.Construction

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/path01.eps" pic1
    >> writeSVG_latin1 "./out/path01.svg" pic1 

-- note draws from background first

pic1 :: Picture Double
pic1 = execDrawing (standardContext 19) $ 
    do { draw circle1
       ; draw curve3
       ; draw curve2
       ; draw curve1
       ; draw eastUpWest
       ; drawAt (P2 110 0) (barb90 0)
       ; drawAt (P2 120 0) (barb60 0)
       ; drawAt (P2 130 0) (barb45 0)
       ; return ()
       }
    
{-
         . barb60 0 std_attr (P2 120 0)
         . barb45 0 std_attr (P2 130 0)
         . eastUpWest
         . curve1
         . curve2
         . curve3
         . circle1
-}


         
curve1 :: Graphic Double
curve1 = openStroke $ curvedPath xs
  where
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 


curve2 :: Graphic Double
curve2 =  localCtxObj (primaryColour red) (openStroke $ toPrimPathU path_one)
  where
    path_one = execPath zeroPt $ curveto 0 (3*pi/2) (P2 60 60)



curve3 :: Graphic Double
curve3 = localCtxObj (primaryColour blue) 
                     (openStroke $ toPrimPathU $ shorten 10 path1)


path1 :: Path Double
path1 = execPath (P2 60 0) $ curveto (pi/2) 0 (P2 0 60)


circle1 :: Graphic Double
circle1 = localCtxObj (secondaryColour yellow) (filledCircle 2 60 zeroPt)

cto4 :: Path Double
cto4 = execPath (P2 180 0) $ curveto (pi/2) 0 (P2 120 60)


-- Note - the distance from the barb ends to the curve is not 
-- evenly spaced
-- 
-- Probably have to average the end tangent and the tangent of 
-- the curve \'shortened\' by the arrow tips back distance.
--

eastUpWest :: Graphic Double
eastUpWest = localCtxObj (primaryColour blue) 
                         (openStroke $ mkP1 (P2 140 0) (P2 160 20))


-- Potentially this may introduce the style that using AGraphic2 
-- would follow.
--
-- i.e. start and end are supplied, line manipulations are done
-- by displacing them.
--  
mkP1 :: Floating u => Point2 u -> Point2 u -> PrimPath u
mkP1 start end = toPrimPathU $ execPath start $ 
                    horizontalVertical (end .+^ hvec 20) >> lineto end

