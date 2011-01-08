{-# OPTIONS -Wall #-}

-- Read this file a make a microprint of it...

module DemoTeletype where


import Wumpus.Microprint.Datatypes
import Wumpus.Microprint.Render
import Wumpus.Microprint.Teletype


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    micro1 <- filePic
    let pic1 = runCtxPictureU std_ctx $ ttDrawing micro1  
    writeEPS "./out/teletype01.eps" pic1
    writeSVG "./out/teletype01.svg" pic1


ttDrawing :: Teletype a -> CtxPicture Double
ttDrawing ma = drawTracing $ do
    renderTeletype sctx borderedF (prefix ma)
  where
    prefix mp = setRGB moccasin >> mp
    sctx      = makeRenderScalingCtx (\x -> fromIntegral $ 6*x) 
                                     (\y -> fromIntegral $ 8*y)


std_ctx :: DrawingContext
std_ctx = standardContext 18


filePic :: IO (Teletype ())
filePic = do
  xs <- readFile "DemoTeletype.hs"
  return $ foldr (\a acc -> drawChar a >> acc) (return ()) xs

drawChar :: Char -> Teletype ()
drawChar '\n' = linebreak
drawChar '\t' = space >> space >> space >> space
drawChar ' '  = space
drawChar _    = char



