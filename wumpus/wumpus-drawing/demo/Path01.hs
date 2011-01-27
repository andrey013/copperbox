{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Arrows.Tips
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.MonadicConstruction

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path01.eps" pic1
    writeSVG "./out/path01.svg" pic1 

path_pic :: DCtxPicture
path_pic = drawTracing $ do
    draw circle1
    draw curve1
    draw curve2
    draw curve3
    draw eastUpWest
    
    

         
curve1 :: Graphic Double
curve1 = openStroke $ curvedPath xs
  where
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 


curve2 :: Graphic Double
curve2 =  localize (strokeColour red) (openStroke $ toPrimPath path_one)
  where
    path_one = execPath zeroPt $ curveto 0 (3*pi/2) (P2 60 60)



curve3 :: Graphic Double
curve3 = localize (strokeColour blue) 
                  (openStroke $ toPrimPath $ shortenPath 10 10 path1)


path1 :: Path Double
path1 = execPath (P2 60 0) $ curveto (pi/2) 0 (P2 0 60)


circle1 :: Graphic Double
circle1 = localize (fillColour gold) (filledCircle 60 `at` zeroPt)

cto4 :: Path Double
cto4 = execPath (P2 180 0) $ curveto (pi/2) 0 (P2 120 60)


-- Note - the distance from the barb ends to the curve is not 
-- evenly spaced
-- 
-- Probably have to average the end tangent and the tangent of 
-- the curve \'shortened\' by the arrow tips back distance.
--

eastUpWest :: Graphic Double
eastUpWest = localize (strokeColour blue) 
                      (openStroke $ mkP1 (P2 140 0) (P2 160 20))


-- Potentially this may introduce the style that using AGraphic2 
-- would follow.
--
-- i.e. start and end are supplied, line manipulations are done
-- by displacing them.
--  
mkP1 :: Floating u => Point2 u -> Point2 u -> PrimPath u
mkP1 start end = toPrimPath $ execPath start $ 
                    horizontalVertical (end .+^ hvec 20) >> lineto end

