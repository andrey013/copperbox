{-# OPTIONS -Wall #-}

-- Read this file a make a microprint of it...

module Demo01 where

import Wumpus.Microprint.Datatypes
import Wumpus.Microprint.Render
import Wumpus.Microprint.Tokenizer

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    inp <- readFile "Demo01.hs"	  -- This file
    let gtext = runTokenizer (haskellTokenizer indian_red teal) inp
    --
    let pic1  = makePicture gtext
    writeEPS "./out/microprint01.eps" pic1
    writeSVG "./out/microprint01.svg" pic1
    --
    let pic2  = makeBordered gtext
    writeEPS "./out/microprint02.eps" pic2
    writeSVG "./out/microprint02.svg" pic2


makePicture :: GreekText -> DPicture
makePicture gtext = liftToPictureU $ execTraceDrawing (standardContext 14) $ 
    render sctx strokelineF gtext
  where
    sctx = makeRenderScalingCtx (\x -> fromIntegral $ 2*x) 
                                (\y -> fromIntegral $ 3*y)

makeBordered :: GreekText -> DPicture
makeBordered gtext = liftToPictureU $ execTraceDrawing (standardContext 14) $ 
    render sctx borderedF gtext
  where
    sctx = makeRenderScalingCtx (\x -> fromIntegral $ 6*x) 
                                (\y -> fromIntegral $ 8*y)
 

