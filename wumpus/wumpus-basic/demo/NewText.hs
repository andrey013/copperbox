{-# OPTIONS -Wall #-}


module NewText where

import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.Advance
import Wumpus.Basic.Text.Datatypes
import Wumpus.Basic.Text.LocBoundingBox

import Wumpus.Core                      -- package: wumpus-core

import Prelude hiding ( pi )

import System.Directory

dummy :: CharBoundingBox AfmUnit
dummy = charBoundingBox (-115) (-240) 1151 1009

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/new_text01.eps" pic1
    writeSVG_latin1 "./out/new_text01.svg" pic1

std_ctx :: DrawingContext
std_ctx = fontface times_roman $ standardContext 48


testLine :: Fractional u => Int -> AdvanceSingle u 
testLine n = makeSingle bbox av (straightLine av)
  where
    width  = fromIntegral n * 12
    bbox   = oLocBoundingBox width 10
    av     = hvec width 

dummyText :: (Fractional u, Ord u) => AdvanceMulti u
dummyText = mk1 5 `align` (mk1 4 `align` mk1 6) 
  where
    align = alignRightH 16 
    mk1   = oneLineH . testLine


pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ 
          drawi_ $ runAdvanceMulti zeroPt dummyText
