{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Drawing.Colour.SVGColours ( steel_blue )
import Wumpus.Drawing.Colour.X11Colours ( indian_red1 )
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

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


fontMsg :: FontDef -> Int -> String
fontMsg ft sz = msgF []
  where
    msgF = showString name . showChar ' ' . shows sz . showString "pt"
    name = ps_font_name $ font_def_face ft 


makeLabel :: RGBi -> FontDef -> Int -> DLocGraphic
makeLabel rgb ft sz = localize upd (dcTextlabel $ fontMsg ft sz)
  where
    upd = text_colour rgb . font_attr ft sz 

-- indian_red1
-- steel_blue

point_sizes :: [Int]
point_sizes = [10, 12, 18, 24, 36, 48]

positions :: [Int]
positions = [0, 12, 27, 49, 78, 122] 

-- Note - this chain might be worth putting in a library...
pointChain :: (Int -> DLocGraphic) -> DLocGraphic
pointChain fn = runChain_ chn_alg $ mapM (onChain . fn) point_sizes
  where
    chn_alg = ChainScheme start step
    start   = \pt -> (pt,point_sizes)

    step _ (pt,[])     = (pt, (displace (vvec 50) pt, []))
    step _ (pt,(y:ys)) = (pt, (displace (vvec $ fromIntegral $ 2 + y)  pt, ys))

fontGraphic :: RGBi -> FontDef -> DLocGraphic 
fontGraphic rgb ft = ignoreAns $ pointChain mkGF
  where
    mkGF sz = makeLabel rgb ft sz


std_ctx :: DrawingContext
std_ctx = standardContext 10


fontDrawing :: [(RGBi,FontDef)] -> CtxPicture
fontDrawing xs = drawTracing $  
    drawl start $ runChain_ chn_alg $ mapM (onChain . uncurry fontGraphic) xs
  where
    chn_alg   = tableDown 4 (1,180)
    start     = P2 0 (4*180)



--------------------------------------------------------------------------------
-- Times

times_cxpic :: CtxPicture
times_cxpic = 
    fontDrawing [ (steel_blue,  times_roman)
                , (indian_red1, times_italic)
                , (steel_blue,  times_bold)
                , (indian_red1, times_bold_italic)
                ] 

helvetica_cxpic :: CtxPicture
helvetica_cxpic = 
    fontDrawing [ (steel_blue,  helvetica)
                , (indian_red1, helvetica_oblique)
                , (steel_blue,  helvetica_bold)
                , (indian_red1, helvetica_bold_oblique)
                ] 



--------------------------------------------------------------------------------

courier_cxpic :: CtxPicture
courier_cxpic = 
    fontDrawing [ (steel_blue,  courier)
                , (indian_red1, courier_oblique)
                , (steel_blue,  courier_bold)
                , (indian_red1, courier_bold_oblique)
                ] 


--------------------------------------------------------------------------------

    
symbol_cxpic :: CtxPicture
symbol_cxpic = 
    fontDrawing [ (steel_blue, symbol) ]
