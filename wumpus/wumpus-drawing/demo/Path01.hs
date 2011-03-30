{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.MonadicConstruction

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path01.eps" pic1
    writeSVG "./out/path01.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ do
    draw circle1
    draw curve1
    draw curve2
    draw curve3
    draw eastUpWest
    
    

         
curve1 :: Graphic Double
curve1 = toPrimPath (curvePath xs) >>= openStroke
  where
    xs :: [DPoint2]
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 


curve2 :: Graphic Double
curve2 =  localize (stroke_colour red) (toPrimPath path_one >>= openStroke)
  where
    path_one = execPath (zeroPt::DPoint2) $ curveto 0 (3*pi/2) (P2 60 60)



curve3 :: Graphic Double
curve3 = localize (stroke_colour blue) 
                  (toPrimPath (shortenPath 10 10 path1) >>= openStroke)


path1 :: Path Double
path1 = execPath (P2 60 0) $ curveto (pi/2) 0 (P2 0 60)


circle1 :: Graphic Double
circle1 = localize (fill_colour gold) (filledCircle 60 `at` zeroPt)

cto4 :: Path Double
cto4 = execPath (P2 180 0) $ curveto (pi/2) 0 (P2 120 60)


-- Note - the distance from the barb ends to the curve is not 
-- evenly spaced
-- 
-- Probably have to average the end tangent and the tangent of 
-- the curve \'shortened\' by the arrow tips back distance.
--

eastUpWest :: Graphic Double
eastUpWest = localize (stroke_colour blue) 
                      (mkP1 (P2 140 0) (P2 160 20) >>= openStroke)


-- Potentially this may introduce the style that using AGraphic2 
-- would follow.
--
-- i.e. start and end are supplied, line manipulations are done
-- by displacing them.
--  
mkP1 :: Point2 Double -> Point2 Double -> Query PrimPath
mkP1 start end = toPrimPath $ execPath start $ 
                    horizontalVertical (end .+^ hvec 20) >> lineto end
  
   

