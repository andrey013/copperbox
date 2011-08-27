{-# OPTIONS -Wall #-}

module PathRel where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.HPath


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
abspath1 = runHPath hp zeroPt
  where
    hp = mconcat [ line_up 60
                 , line_up_right 40 
                 , line_down_right 40
                 , line_down 60
                 , line_left 20
                 , line_up   30
                 , curve (vvec 20) (vec (-10) 10) (hvec (-10))
                 , curve (hvec (-10)) (vec (-10) (-10)) (vvec (-20))
                 , line_down 30
                 ]
                            
                          
