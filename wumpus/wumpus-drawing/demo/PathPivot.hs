{-# OPTIONS -Wall #-}

module PathPivot where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import System.Directory
import Prelude hiding ( lines )

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_pivot.eps" pic1
    writeSVG "./out/path_pivot.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ localize (set_line_width 2) $ do
    drawl (P2 0 0)   $ duplicateH 4 80 (dcDisk DRAW_FILL 4)
    drawl (P2 0 0)   $ distribH 80 [path1, path2, path3, path4]
    return ()  
    

path1 :: LocGraphic Double
path1 = runPivot (penlines []) (penlines [go_up 20 , go_up_right 20, go_down 60])

path2 :: LocGraphic Double
path2 = runPivot (penlines [go_up 20]) (penlines [go_up_right 20, go_down 60])

path3 :: LocGraphic Double
path3 = runPivot (penlines [go_up 20, go_up_right 20]) (penlines [go_down 60])

path4 :: LocGraphic Double
path4 = runPivot (penlines [go_up 20, go_up_right 20, go_down 60]) (penlines [])


