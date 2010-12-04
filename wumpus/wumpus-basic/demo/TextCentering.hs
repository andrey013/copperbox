{-# OPTIONS -Wall #-}

module TextCentering where

import Wumpus.Basic.Graphic
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Colour.SVGColours

import Wumpus.Core                      -- package: wumpus-core


import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_attr text_drawing
    writeEPS "./out/text_centering01.eps" pic1
    writeSVG "./out/text_centering01.svg" pic1

-- ignore :: Monad m => m a -> m ()
-- ignore ma = ma >> return ()


std_attr :: DrawingContext
std_attr = standardContext 24

text_drawing :: DDrawing
text_drawing = drawTracing $ do
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
coordinate = localize (halfsize . strokeColour red) (postpro1 snd dotPlus)

textSquare :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
textSquare ss =
     monoTextDimensions ss  >>= \(w,h) ->
     monoDescenderDepth     >>= \dy    ->
     localize (strokeColour medium_sea_green . dashPattern dp)
              (prepro1 (vdisplace (-dy)) $ strokedRectangle w h)
  where
    dp = doublegaps unit_dash_pattern 


textSquareCtr :: (Fractional u, Ord u, FromPtSize u) 
              => String -> LocGraphic u
textSquareCtr ss =  
    monoTextDimensions ss >>= \(w,h) ->
    localize (strokeColour dim_gray . dashPattern dp)
             (prepro1 (mkmove w h) (strokedRectangle w h))
  where
    dp         = doublegaps unit_dash_pattern 
    mkmove w h = displace (0.5*(-w)) (0.5*(-h))

