{-# OPTIONS -Wall #-}

module PathPivot where

import Wumpus.Drawing.Paths.Relative

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_pivot.eps" pic1
    writeSVG "./out/path_pivot.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ localize (set_line_width 2) $ do
    drawl (P2 0 0)   $ duplicateH 3 80 (filledDisk 4)
    drawl (P2 0 0)   $ distribH 80 $ map execPivot [path1, path2, path3, path4]
    return ()  
    

path1 :: RelBuild Double ()
path1 = line_up 20 >> line_up_right 20 >> line_down 60

path2 :: RelBuild Double ()
path2 = line_up 20 >> pivot >> line_up_right 20 >> line_down 60

path3 :: RelBuild Double ()
path3 = line_up 20 >> line_up_right 20 >> pivot >> line_down 60

path4 :: RelBuild Double ()
path4 = line_up 20 >> line_up_right 20 >> line_down 60 >> pivot

