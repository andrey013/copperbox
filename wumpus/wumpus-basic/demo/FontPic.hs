{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Basic.Chains
import Wumpus.Basic.Graphic
import Wumpus.Basic.Colour.SVGColours ( steel_blue )
import Wumpus.Basic.Colour.X11Colours ( indian_red1 )
import Wumpus.Basic.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/font_courier.eps"   courier_pic
    writeSVG_latin1 "./out/font_courier.svg"   courier_pic
    writeEPS_latin1 "./out/font_times.eps"     times_pic
    writeSVG_latin1 "./out/font_times.svg"     times_pic
    writeEPS_latin1 "./out/font_helvetica.eps" helvetica_pic
    writeSVG_latin1 "./out/font_helvetica.svg" helvetica_pic
    writeEPS_latin1 "./out/font_symbol.eps"    symbol_pic
    writeSVG_latin1 "./out/font_symbol.svg"    symbol_pic


fontMsg :: FontFace -> Int -> String
fontMsg ff sz = msgF []
  where
    msgF = showString (font_name ff) . showChar ' ' . shows sz . showString "pt"


makeLabel :: RGBi -> FontFace -> Int -> DLocGraphic
makeLabel rgb ff sz = localize upd . (textline $ fontMsg ff sz)
  where
    upd = fillColour rgb . fontsize sz . fontface ff

-- indian_red1
-- steel_blue

point_sizes :: [Int]
point_sizes = [10, 12, 18, 24, 36, 48]

positions :: [Int]
positions = [0, 12, 27, 49, 78, 122] 


pointChain :: LocChain Int Int Double
pointChain = verticals positions

fontGraphic :: RGBi -> FontFace -> DPoint2 -> Drawing Double ()
fontGraphic rgb ff pt = 
    let ps = unchain (coordinateScalingContext 1 1) $ pointChain pt in 
      zipWithM_ (\p1 sz -> draw $ makeLabel rgb ff sz `at` p1) ps point_sizes


std_ctx :: DrawingContext
std_ctx = standardContext 10


fontPicture :: [(RGBi,FontFace)] -> DPicture
fontPicture xs = liftToPictureU $ execDrawing std_ctx $  
                   zipWithM (\(rgb,ff) pt -> fontGraphic rgb ff pt) xs ps
  where
    ps = unchain (coordinateScalingContext 1 180) $ tableDown 4 1



--------------------------------------------------------------------------------
-- Times

times_pic :: Picture Double
times_pic = 
    fontPicture [ (steel_blue,  timesRoman)
                , (indian_red1, timesItalic)
                , (steel_blue,  timesBold)
                , (indian_red1, timesBoldItalic)
                ] 

helvetica_pic :: Picture Double
helvetica_pic = 
    fontPicture [ (steel_blue,  helvetica)
                , (indian_red1, helveticaOblique)
                , (steel_blue,  helveticaBold)
                , (indian_red1, helveticaBoldOblique)
                ] 



--------------------------------------------------------------------------------

courier_pic :: Picture Double
courier_pic = 
    fontPicture [ (steel_blue,  courier)
                , (indian_red1, courierOblique)
                , (steel_blue,  courierBold)
                , (indian_red1, courierBoldOblique)
                ] 


--------------------------------------------------------------------------------

    
symbol_pic :: Picture Double
symbol_pic = 
    fontPicture [ (steel_blue, symbol) ]
