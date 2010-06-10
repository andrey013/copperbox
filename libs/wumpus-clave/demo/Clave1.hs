{-# OPTIONS -Wall #-}

module Clave1 where

import Wumpus.Clave.Core        
import Wumpus.Clave.Drawing

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Data.Maybe
import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/clave01.eps" pic1 
    >> writeSVG_latin1 "./out/clave01.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = fromMaybe errK $ drawGraphic line1

    errK = error "no picture"

line1 :: DGraphic
line1 = circleF 24 black (P2 0 0) . barF 24 black (P2 24 0)



