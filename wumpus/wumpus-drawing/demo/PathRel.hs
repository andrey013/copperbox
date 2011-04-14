{-# OPTIONS -Wall #-}

module PathRel where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Relative

import qualified Wumpus.Drawing.Paths.Absolute as A

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
    drawl (P2 0 150) $ path2
    return ()  
    


path1 :: DLocGraphic
path1 = localize (stroke_colour red) $ execRelBuild path_spec1



path2 :: DLocGraphic 
path2 = localize (stroke_colour red) $  promoteR1 $ \pt -> 
   let relp = evalRelBuild path_spec1 
   in toPrimPath pt relp >>= filledPath


path_spec1 :: RelBuild Double ()
path_spec1 =  
       rlineto (V2 0 50)
    >> rlineto (V2 50 0)
    >> insert disk1
    >> rmoveto (V2 0 (-50))
    >> rlineto (V2 100 0) 
    >> vamp   (V2 40 40) (dotted_line) square
    >> rlineto (V2 20 0)
         
--    (stroke_colour blue)
    >> rlineto (V2 50 0)
    >> rlineto (V2 0 (-40))
          
 
  where
    disk1 = strokedDisk 10



-- What about close / cycle ?
--
square :: RelPath Double
square = lineTo (hvec 40) `append` lineTo (vvec 40) 
    `append` lineTo (hvec $ negate 40) `append` lineTo (vvec $ negate 40)
