{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours ( steel_blue )
import Wumpus.Drawing.Colour.X11Colours ( indian_red1 )
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core


import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    --
    let courier_pic = runCtxPictureU std_ctx courier_cxpic
    writeEPS "./out/font_courier.eps"   courier_pic
    writeSVG "./out/font_courier.svg"   courier_pic
    --
    let times_pic = runCtxPictureU std_ctx times_cxpic
    writeEPS "./out/font_times.eps"     times_pic
    writeSVG "./out/font_times.svg"     times_pic
    --
    let helvetica_pic = runCtxPictureU std_ctx helvetica_cxpic
    writeEPS "./out/font_helvetica.eps" helvetica_pic
    writeSVG "./out/font_helvetica.svg" helvetica_pic
    --
    let symbol_pic = runCtxPictureU std_ctx symbol_cxpic
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


pointChain :: LocChain Double
pointChain = verticals $ map (fromIntegral . (+2)) point_sizes

fontGraphic :: RGBi -> FontFace -> DPoint2 -> TraceDrawing Double ()
fontGraphic rgb ff pt = 
    let ps = pointChain pt in 
      zipchainWith (\sz -> makeLabel rgb ff sz) point_sizes ps


std_ctx :: DrawingContext
std_ctx = standardContext 10


fontDrawing :: [(RGBi,FontFace)] -> DCtxPicture
fontDrawing xs = drawTracing $  
    zipchainWithTD (\(rgb,ff) -> fontGraphic rgb ff) xs ps
  where
    ps = tableDown 4 (1,180) (P2 0 (4*180))



--------------------------------------------------------------------------------
-- Times

times_cxpic :: CtxPicture Double
times_cxpic = 
    fontDrawing [ (steel_blue,  times_roman)
                , (indian_red1, times_italic)
                , (steel_blue,  times_bold)
                , (indian_red1, times_bold_italic)
                ] 

helvetica_cxpic :: CtxPicture Double
helvetica_cxpic = 
    fontDrawing [ (steel_blue,  helvetica)
                , (indian_red1, helvetica_oblique)
                , (steel_blue,  helvetica_bold)
                , (indian_red1, helvetica_bold_oblique)
                ] 



--------------------------------------------------------------------------------

courier_cxpic :: CtxPicture Double
courier_cxpic = 
    fontDrawing [ (steel_blue,  courier)
                , (indian_red1, courier_oblique)
                , (steel_blue,  courier_bold)
                , (indian_red1, courier_bold_oblique)
                ] 


--------------------------------------------------------------------------------

    
symbol_cxpic :: CtxPicture Double
symbol_cxpic = 
    fontDrawing [ (steel_blue, symbol) ]
