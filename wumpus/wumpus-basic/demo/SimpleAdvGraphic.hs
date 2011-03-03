{-# OPTIONS -Wall #-}


module SimpleAdvGraphic where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/simple_adv_graphic01.eps" pic1
    writeSVG "./out/simple_adv_graphic01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing UDouble $ mf 



-- TraceDrawing can be fully unit polymorphic
mf :: PtSize u => TraceDrawing u ()
mf = do
    drawi_ $ advspace (hvec 10) [text01, text02, text01] `at` P2 0 120
    drawi_ $ advconcat [text01, text02, text01] `at` P2 0 80
    drawi_ $ (miniDisk `advcat` text01 `advcat` miniDisk) `at` P2 0 40 
    drawi_ $ (miniDisk `advcat` text02 `advcat` miniDisk) `at` P2 0 0 


-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: PtSize u => AdvGraphic u 
text01 = replaceAns (hvec 84) $ textline "text01"
    

text02 :: PtSize u => AdvGraphic u 
text02 = replaceAns (hvec 210) $ textline "text number two"


miniDisk :: PtSize u => AdvGraphic u
miniDisk = replaceAns (V2 0 0) $ localize (fill_colour sienna) $ filledDisk 3


sienna :: RGBi
sienna = RGBi 160 82 45