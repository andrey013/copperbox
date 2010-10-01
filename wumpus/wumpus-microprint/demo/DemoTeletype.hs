{-# OPTIONS -Wall #-}

-- Read this file a make a microprint of it...

module DemoTeletype where

import Wumpus.Microprint

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Colour.SVGColours           -- package: wumpus-basic

import Data.Maybe
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    micro1 <- filePic
    let pic1 = fromMaybe errK $ renderTeletype sctx borderedF (prefix micro1)
    writeEPS_latin1 "./out/teletype01.eps" pic1
    writeSVG_latin1 "./out/teletype01.svg" pic1
  where
    prefix mp = setRGB moccasin >> mp
    sctx      = makeRenderScaling (\x -> fromIntegral $ 6*x) 
                                  (\y -> fromIntegral $ 8*y)


errK :: a
errK = error "no picture"

filePic :: IO (Teletype ())
filePic = do
  xs <- readFile "DemoTeletype.hs"
  return $ foldr (\a acc -> drawChar a >> acc) (return ()) xs

drawChar :: Char -> Teletype ()
drawChar '\n' = linebreak
drawChar '\t' = space >> space >> space >> space
drawChar ' '  = space
drawChar _    = char



