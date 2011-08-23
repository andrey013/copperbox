{-# OPTIONS -Wall #-}

module PathRel where

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
path1 = promoteLoc $ \(P2 x y) -> translate x y (drawOpenPath_ abspath1) 


dots1 :: DLocGraphic 
dots1 = localize (fill_colour red) $ promoteLoc $ \(P2 x y) ->
    mconcat $ map (\pt -> smallDisk `at` translate x y pt) $ dots_list

dots_list :: [DPoint2]
dots_list = map fst $ pathdiv 30 20 10 abspath1



abspath1 :: AbsPath Double
abspath1 = emptyPath zeroPt `snocLineTo` (P2 0 60) 
                            `snocLineTo` (P2 40 100)
                            `snocLineTo` (P2 80 60)
                            `snocLineTo` (P2 80 0)
                            `snocLineTo` (P2 60 0)  
                            `snocLineTo`  (P2 60 30)
                            `snocCurveTo` (P2 60 50, P2 50 60, P2 40 60)
                            `snocCurveTo` (P2 30 60, P2 20 50, P2 20 30)
                            `snocLineTo` (P2 20 0)
                          
