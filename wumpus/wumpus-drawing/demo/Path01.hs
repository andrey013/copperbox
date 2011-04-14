{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Absolute
import qualified Wumpus.Drawing.Paths.Relative as Rel


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import Prelude hiding ( cycle )
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
    draw triangles
    draw curve1
    draw curve2
    draw curve3
    draw eastUpWest
    

triangles :: Graphic Double
triangles = localize (set_line_width 8) $ execAbsBuild (P2 0 0) $ 
    penColour dark_slate_blue >>
    moveto (P2  60 0) >> tristeps >>
    moveto (P2 120 0) >> tristeps >>
    moveto (P2 180 0) >> tristeps >> cycle >>
    moveto (P2 240 0) >> tristeps >> cycle
  where
    tristeps :: AbsBuild Double ()
    tristeps = rlineto (V2 40 0) >> rlineto (V2 0 40) >> rlineto (V2 (-40) (-40))
       

         
curve1 :: Graphic Double
curve1 = toPrimPath (curvePath xs) >>= openStroke
  where
    xs :: [DPoint2]
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 


curve2 :: Graphic Double
curve2 =  localize (stroke_colour red) (toPrimPath path_one >>= openStroke)
  where
    path_one = evalAbsBuild (zeroPt::DPoint2) $ ctrlcurveto 0 (3*pi/2) (P2 60 60)



curve3 :: Graphic Double
curve3 = localize (stroke_colour blue) 
                  (toPrimPath (shortenPath 10 10 path1) >>= openStroke)


path1 :: AbsPath Double
path1 = evalAbsBuild (P2 60 0) $ ctrlcurveto (pi/2) 0 (P2 0 60)


circle1 :: Graphic Double
circle1 = localize (fill_colour gold) (filledCircle 60 `at` zeroPt)

cto4 :: AbsPath Double
cto4 = evalAbsBuild (P2 180 0) $ ctrlcurveto (pi/2) 0 (P2 120 60)


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
mkP1 start end@(P2 x1 y1) = 
    toPrimPath $ evalAbsBuild start $ 
      horizontalVertical (end .+^ hvec 20) >> lineto (P2 x1 y1)
  

horizontalVertical :: Floating u => Point2 u -> AbsBuild u ()
horizontalVertical (P2 x y) = 
    tip >>= \(P2 _ y0) -> lineto (P2 x y0) >> lineto (P2 x y)

   

