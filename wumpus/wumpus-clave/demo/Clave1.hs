{-# OPTIONS -Wall #-}

module Clave1 where

import Wumpus.Clave.Drawing

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours

import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/clave01.eps" pic1
    writeSVG "./out/clave01.svg" pic1 


{-

pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic line1

errK :: a
errK = error "no picture"

line1 :: DGraphic
line1 = circleF 24 black (P2 0 0) 
                . barF 24 black (P2 24 0) 
                . gridF 2 24 0.5 (P2 0 0) 


pic2 :: DPicture
pic2 = fromMaybe errK $ drawGraphic $ 
        evalClaveM cfg (beat >> rest >> highlight 2 sandy_brown 
                             >> beat >> rest >> rest >> endLine)
  where
    cfg = ClaveConfig { box_height      = 24
                      , scalefun        = (24.0 *) . fromIntegral }

-}
