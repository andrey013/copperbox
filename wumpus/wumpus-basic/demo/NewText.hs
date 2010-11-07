{-# OPTIONS -Wall #-}


module NewText where

import Wumpus.Basic.Colour.SVGColours
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


type CatF u = AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u


dummyText :: (Fractional u, Ord u) => CatF u -> AdvanceMulti u
dummyText op = (((mk1 10 `op` mk1 4) `op` mk1 5) `op` mk1 6) `op` mk1 4
  where
    mk1   = oneLineH . testLine


pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ do
          drawi_ $ right_text  `at` P2 0 200
          drawi_ $ left_text   `at` P2 0 100
          drawi_ $ center_text `at` P2 0   0



right_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
right_text = localize (strokeColour red) $ 
               runAdvanceMulti (dummyText $ alignRightH 16)

left_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
left_text = localize (strokeColour green) $ 
               runAdvanceMulti (dummyText $ alignLeftH 16)

center_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
center_text = localize (strokeColour blue) $ 
               runAdvanceMulti (dummyText $ alignCenterH 16)
