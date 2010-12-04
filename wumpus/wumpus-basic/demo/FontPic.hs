{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Basic.Graphic
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours ( steel_blue )
import Wumpus.Drawing.Colour.X11Colours ( indian_red1 )
import Wumpus.Drawing.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    --
    let courier_pic = runDrawingU std_ctx courier_drawing
    writeEPS "./out/font_courier.eps"   courier_pic
    writeSVG "./out/font_courier.svg"   courier_pic
    --
    let times_pic = runDrawingU std_ctx times_drawing
    writeEPS "./out/font_times.eps"     times_pic
    writeSVG "./out/font_times.svg"     times_pic
    --
    let helvetica_pic = runDrawingU std_ctx helvetica_drawing
    writeEPS "./out/font_helvetica.eps" helvetica_pic
    writeSVG "./out/font_helvetica.svg" helvetica_pic
    --
    let symbol_pic = runDrawingU std_ctx symbol_drawing
    writeEPS "./out/font_symbol.eps"    symbol_pic
    writeSVG "./out/font_symbol.svg"    symbol_pic


fontMsg :: FontFace -> Int -> String
fontMsg ff sz = msgF []
  where
    msgF = showString (ps_font_name ff) . showChar ' ' . shows sz . showString "pt"


makeLabel :: RGBi -> FontFace -> Int -> DLocGraphic
makeLabel rgb ff sz = localize upd (textline $ fontMsg ff sz)
  where
    upd = fillColour rgb . fontAttr ff sz 

-- indian_red1
-- steel_blue

point_sizes :: [Int]
point_sizes = [10, 12, 18, 24, 36, 48]

positions :: [Int]
positions = [0, 12, 27, 49, 78, 122] 


pointChain :: LocChain Int Int Double
pointChain = verticals positions

fontGraphic :: RGBi -> FontFace -> DPoint2 -> TraceDrawing Double ()
fontGraphic rgb ff pt = 
    let ps = unchain (coordinateScalingContext 1 1) $ pointChain pt in 
      zipWithM_ (\p1 sz -> draw $ makeLabel rgb ff sz `at` p1) ps point_sizes


std_ctx :: DrawingContext
std_ctx = standardContext 10


fontDrawing :: [(RGBi,FontFace)] -> DDrawing
fontDrawing xs = drawTracing $  
    zipWithM (\(rgb,ff) pt -> fontGraphic rgb ff pt) xs ps
  where
    ps = unchain (coordinateScalingContext 1 180) $ tableDown 4 1



--------------------------------------------------------------------------------
-- Times

times_drawing :: Drawing Double
times_drawing = 
    fontDrawing [ (steel_blue,  times_roman)
                , (indian_red1, times_italic)
                , (steel_blue,  times_bold)
                , (indian_red1, times_bold_italic)
                ] 

helvetica_drawing :: Drawing Double
helvetica_drawing = 
    fontDrawing [ (steel_blue,  helvetica)
                , (indian_red1, helvetica_oblique)
                , (steel_blue,  helvetica_bold)
                , (indian_red1, helvetica_bold_oblique)
                ] 



--------------------------------------------------------------------------------

courier_drawing :: Drawing Double
courier_drawing = 
    fontDrawing [ (steel_blue,  courier)
                , (indian_red1, courier_oblique)
                , (steel_blue,  courier_bold)
                , (indian_red1, courier_bold_oblique)
                ] 


--------------------------------------------------------------------------------

    
symbol_drawing :: Drawing Double
symbol_drawing = 
    fontDrawing [ (steel_blue, symbol) ]
