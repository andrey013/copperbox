{-# OPTIONS -Wall #-}

-- Read this file a make a microprint of it...

module Demo01 where

import Wumpus.MicroPrint

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Colour.SVGColours           -- package: wumpus-basic

import Data.Maybe
import System.Directory

main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; micro1 <- filePic
    ; let pic1 = fromMaybe errK $ renderMicroPrint cfg1 (prefix micro1)
    ; writeEPS_latin1 "./out/mp01.eps" pic1
    ; writeSVG_latin1 "./out/mp01.svg" pic1
    }
  where
    prefix mp = setRGB moccasin >> mp

errK :: a
errK = error "no picture"

filePic :: IO (MicroPrint ())
filePic = do
  xs <- readFile "Demo01.hs"
  return $ foldr (\a acc -> drawChar a >> acc) (return ()) xs

drawChar :: Char -> MicroPrint ()
drawChar '\n' = linebreak
drawChar '\t' = space >> space >> space >> space
drawChar ' '  = space
drawChar _    = char


cfg1 :: MicroPrintConfig
cfg1 = MicroPrintConfig 
       { char_height    = 12.0
       , char_width     = 8.0
       , line_spacing   = 3.0
       , drawWordF      = borderedF
       }
 

