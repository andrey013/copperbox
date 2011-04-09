{-# OPTIONS -Wall #-}


module SimpleAdvGraphic where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
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
drawing01 = drawTracing mf 



-- Although TraceDrawing can be fully unit polymorphic, it seems 
-- always best to specialize as we are stating concrete values
-- (and they will be in some unit).
--

mf :: TraceDrawing Double ()
mf = do
    drawl (P2 0 120) $ 
        runAdvanceObject $ evenspace (hvec 10) [text01, text02, text01]

    drawl (P2 0 80) $ 
        runAdvanceObject $ nexts [text01, text02, text01]

    drawl (P2 0 40) $ 
        runAdvanceObject (miniDisk `next` text01 `next` miniDisk)

    drawl (P2 0 0) $
        runAdvanceObject (miniDisk `next` text02 `next` miniDisk)


-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: AdvanceObject Double
text01 = makeAdvanceObject (pure $ hvec 84) $ plainTextLine "text01"
    

text02 :: AdvanceObject Double
text02 = makeAdvanceObject (pure $ hvec 210) $ plainTextLine "text number two"


miniDisk :: AdvanceObject Double
miniDisk = makeAdvanceObject (pure $ V2 0 0) disk1 
  where
   disk1 = localize (fill_colour sienna) $ filledDisk 3


sienna :: RGBi
sienna = RGBi 160 82 45