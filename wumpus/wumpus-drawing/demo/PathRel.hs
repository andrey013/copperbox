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
--    drawl (P2 400 0) $ makePD path_spec2
--    drawl (P2 0 200) $ makePD path_spec3
--    drawl (P2 100 200) $ makePD path_spec4

    return ()  
    


path1 :: DLocGraphic
path1 = localize (stroke_colour dark_red) $ execPathSpec path_spec1



path2 :: DLocGraphic 
path2 = ignoreAns $ localize (stroke_colour red) $ 
    obliterate2 (evalPathSpec path_spec1) >>= drawClosedPath FILL

makePD :: PathSpec Double a -> DLocGraphic
makePD spec = localize (stroke_colour red) $ ignoreAns $ execPathSpec spec


-- Note - current definition of @obliterate@ in Wumpus-Basic is
-- not satisfactory. A better definition would replace the 
-- collected CatPrim with mempty.
--
-- This is a place holder.
--
obliterate2 :: a -> a
obliterate2 = id

path_spec1 :: PathSpec Double (UNil Double)
path_spec1 = do
    lineto   (V2 0 50)
    lineto   (V2 50 0)
    insertl  disk1
    moveBy   (V2 0 (-50))
    lineto   (V2 100 0) 
    vamp     vamp1  
    lineto   (V2 20 0)

    localPen (stroke_colour blue) $ do 
         { breakPath 
         ; lineto   (V2 50 0)
         ; lineto   (V2 0 (-40))
         ; cycleSubPath FILL_STROKE
         }
    ureturn
  where
    disk1 = dcDisk STROKE 10

    vamp1 = squareWE 40 


ureturn :: Monad m => m (UNil u)
ureturn = return UNil