{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


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
    drawl (P2  50 0) $ circle1
    drawl (P2 150 0) $ triangles
    draw $ renderPath_ CSTROKE $ roundExterior 10 $ 
            anaTrailPath (P2 0 200) (rectangleTrail 50 40)
--    draw curve02
--    draw curve03
    

triangles :: LocGraphic Double
triangles = localize (set_line_width 8) $ (runPathSpec_ OSTROKE) $ 
    localize (stroke_colour dark_slate_blue) $ 
    moveby (hvec 60) >> tristeps >>
    moveby (hvec 60) >> tristeps >>
    moveby (hvec 60) >> tristeps >> cycleSubPath DRAW_STROKE >>
    moveby (hvec 60) >> tristeps >> cycleSubPath DRAW_STROKE >>
    ureturn
  where
    tristeps :: PathSpec Double ()
    tristeps = penline (V2 40 0) >> penline (V2 0 40) >> penline (V2 (-40) (-40))
       

{-
         
curve01 :: Graphic Double
curve01 = toPrimPath (curvePP xs) >>= dcOpenPath
  where
    xs :: [DPoint2]
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 


curve02 :: Graphic Double
curve02 =  localize (stroke_colour red) $ promoteR1 $ \pt -> 
    toPrimPath pt path_one >>= dcOpenPath
  where
    path_one = evalPathSpec $ ctrlcurve 0 (3*pi/2) (P2 60 60)



curve03 :: LocGraphic Double
curve03 = localize (stroke_colour blue) $ promoteR1 $ \pt -> 
                   (toPrimPath pt path1 {- (shortenPath 10 10 path1) -} >>= dcOpenPath)


path1 :: RelPath Double
path1 = evalPathSpec $ ctrlcurve (pi/2) 0 (P2 0 60)

-}

circle1 :: LocGraphic Double
circle1 = localize (fill_colour gold) (dcCircle DRAW_FILL 60)

