{-# OPTIONS -Wall #-}

module TextCentering where

import Wumpus.Basic.Dots
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core


import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10

ignore :: Monad m => m a -> m ()
ignore ma = ma >> return ()

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/text_centering01.eps" pic1
    writeSVG_latin1 "./out/text_centering01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 24) $ do
          draw $ textline sample_text `at` zeroPt
          draw $ coordinate `at` zeroPt
          draw $ textSquare sample_text `at` zeroPt

          draw $ centermonoTextline sample_text `at` p2
          draw $ coordinate `at` p2
          draw $ textSquareCtr sample_text `at` p2
  where
    sample_text = "Sample text"
    p2          = P2 260 8



coordinate :: (Floating u, FromPtSize u) => LocGraphic u
coordinate = localDF (halfsize . primaryColour red) . (fmap snd . dotPlus)

textSquare :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
textSquare ss pt = 
     textDimensions ss  >>= \(w,h) ->
     monoDescenderDepth >>= \dy    ->
     localDF (primaryColour medium_sea_green . dashPattern dp)
             (strokedRectangle w h (vdisplace (-dy) pt))
  where
    dp = doublegaps unit_dash_pattern 


textSquareCtr :: (Fractional u, Ord u, FromPtSize u) 
              => String -> LocGraphic u
textSquareCtr ss pt = 
    textDimensions ss >>= \(w,h) ->
    localDF (primaryColour dim_gray . dashPattern dp)
            (strokedRectangle w h (displace (0.5*(-w)) (0.5*(-h)) pt))
  where
    dp = doublegaps unit_dash_pattern 


