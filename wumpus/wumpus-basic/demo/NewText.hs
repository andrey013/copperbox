{-# OPTIONS -Wall #-}


module NewText where

import NimbusRomanMetricsGPL

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.Advance
import Wumpus.Basic.Text.Datatypes

import Wumpus.Core                      -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

demo01 :: IO ()
demo01 = do 
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1

std_ctx :: DrawingContext
std_ctx = fontface times_roman $ standardContext 18


type CatF u = AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u

dummyText :: (Fractional u, Ord u, FromPtSize u) 
          => CatF u -> AdvanceMulti u
dummyText op = (((mk1 "One Two Three" `op` mk1 "Four") 
                  `op` mk1 "Five") `op` mk1 "Six") `op` mk1 "Seven"
  where
    mk1 ss = oneLineH $ singleLine ss ptsize nimbus_metrics
    ptsize = fromIntegral $ font_size $ font_props std_ctx


pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ do
          drawi_ $ right_text  `at` P2 0 200
          drawi_ $ left_text   `at` P2 0 100
          drawi_ $ center_text `at` P2 0   0



right_text :: (Fractional u, Ord u, FromPtSize u) 
           => LocImage u (BoundingBox u)
right_text = localize (strokeColour red) $ 
               runAdvanceMulti (dummyText $ alignRightH 16)

left_text :: (Fractional u, Ord u, FromPtSize u) 
          => LocImage u (BoundingBox u)
left_text = localize (strokeColour green) $ 
               runAdvanceMulti (dummyText $ alignLeftH 16)

center_text :: (Fractional u, Ord u, FromPtSize u) 
            => LocImage u (BoundingBox u)
center_text = localize (strokeColour blue) $ 
               runAdvanceMulti (dummyText $ alignCenterH 16)


