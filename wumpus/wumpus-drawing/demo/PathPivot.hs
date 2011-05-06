{-# OPTIONS -Wall #-}

module PathPivot where

import Wumpus.Drawing.Paths.Relative

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
    drawl (P2 0 0)   $ duplicateH 4 80 (dcDisk FILL 4)
    drawl (P2 0 0)   $ distribH 80 [path1, path2, path3, path4]
    return ()  
    

path1 :: LocGraphic Double
path1 = execPivot (lines []) (lines [go_up 20 , up_right 20, go_down 60])

path2 :: LocGraphic Double
path2 = execPivot (lines [go_up 20]) (lines [up_right 20, go_down 60])

path3 :: LocGraphic Double
path3 = execPivot (lines [go_up 20, up_right 20]) (lines [go_down 60])

path4 :: LocGraphic Double
path4 = execPivot (lines [go_up 20, up_right 20, go_down 60]) (lines [])



-- This looks like a good idiom!
-- 
-- Better to put the vocalulary of moves into vectors then every 
-- that takes vectors as arguments an use them.
--
up_right :: u -> Vec2 u
up_right du = V2 du du

go_up :: Num u => u -> Vec2 u
go_up du = V2 0 du

go_down :: Num u => u -> Vec2 u
go_down du = V2 0 (-du)
