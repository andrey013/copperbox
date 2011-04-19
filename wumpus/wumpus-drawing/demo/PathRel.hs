{-# OPTIONS -Wall #-}

module PathRel where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Paths.Vamps


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_rel.eps" pic1
    writeSVG "./out/path_rel.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ do
    drawl (P2 0 0)   $ path1
    drawl (P2 0 100) $ path2
    drawl (P2 400 0) $ makePD path_spec2
    drawl (P2 0 200) $ makePD path_spec3
    drawl (P2 100 200) $ makePD path_spec4

    return ()  
    


path1 :: DLocGraphic
path1 = localize (stroke_colour dark_red) $ execRelBuild path_spec1



path2 :: DLocGraphic 
path2 = localize (stroke_colour red) $  promoteR1 $ \pt -> 
   let relp = evalRelBuild path_spec1 
   in toPrimPath pt relp >>= filledPath

makePD :: RelBuild Double () -> DLocGraphic
makePD spec = localize (stroke_colour red) $ execRelBuild spec



path_spec1 :: RelBuild Double ()
path_spec1 =  
       line (V2 0 50)
    >> line (V2 50 0)
    >> insert disk1
    >> move (V2 0 (-50))
    >> line (V2 100 0) 
    >> vamp vamp1  
    >> line (V2 20 0)
         
    >> pen_colour blue
    >> line (V2 50 0)
    >> line (V2 0 (-40))
          
 
  where
    disk1 = strokedDisk 10

    vamp1 = Vamp { vamp_move_span  = V2 40 40
                 , vamp_move_start = V2 0 0
                 , vamp_dc_update  = dotted_line
                 , vamp_deco_path  = square
                 , vamp_path_end   = PATH_CLOSED 
                 }



path_spec2 :: RelBuild Double ()
path_spec2 = hline (-30) >> vamp (circleVamp $ hvec (-30)) >> hline (-30)

path_spec3 :: RelBuild Double ()
path_spec3 = hline 30 >> vamp (circleVamp $ hvec 30) >> hline 30


path_spec4 :: RelBuild Double ()
path_spec4 = line_up_right 25 >> vamp (circleVamp $ vec 25 25) >> line_up_right 25


-- What about close / cycle ?
--
square :: RelPath Double
square = line1 (hvec 40) `append` line1 (vvec 40) 
    `append` line1 (hvec $ negate 40) `append` line1 (vvec $ negate 40)
