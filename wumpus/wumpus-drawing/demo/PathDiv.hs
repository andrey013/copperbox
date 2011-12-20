{-# OPTIONS -Wall #-}

module PathDiv where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Paths


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import Data.Monoid
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_div.eps" pic1
    writeSVG "./out/path_div.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ do
    drawl (P2 0 0)   $ path1
    drawl (P2 140 0) $ path1
    drawl (P2 140 0) $ dots1
    return ()  
    


path1 :: DLocGraphic
path1 = promoteLoc $ \(P2 x y) -> translate x y (renderPath_ OSTROKE abspath1) 


dots1 :: DLocGraphic 
dots1 = localize (fill_colour red) $ promoteLoc $ \(P2 x y) ->
    mconcat $ map (\pt -> smallDisk `at` translate x y pt) $ dots_list

dots_list :: [DPoint2]
dots_list = map fst $ pathdiv 30 20 10 abspath1



abspath1 :: AbsPath Double
abspath1 = catTrailPath zeroPt hp
  where
    hp = mconcat [ trail_up 60
                 , trail_up_right 40 
                 , trail_down_right 40
                 , trail_down 60
                 , trail_left 20
                 , trail_up   30
                 , catcurve (vvec 20) (vec (-10) 10) (hvec (-10))
                 , catcurve (hvec (-10)) (vec (-10) (-10)) (vvec (-20))
                 , trail_down 30
                 ]
                            
                          
