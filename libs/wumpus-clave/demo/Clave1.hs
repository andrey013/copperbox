{-# OPTIONS -Wall #-}

module Clave1 where

import Wumpus.Clave.ClaveMonad
import Wumpus.Clave.Core        
import Wumpus.Clave.Drawing

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Extra.SVGColours

import Data.Maybe
import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/clave01.eps" pic2
    >> writeSVG_latin1 "./out/clave01.svg" pic2 


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
        evalClaveM cfg (beat >> rest >> highlight 2 sandyBrown 
                             >> rest >> beat >> rest >> endLine)
  where
    cfg = ClaveConfig { box_height      = 24
                      , scalefun        = (24.0 *) . fromIntegral }


----------
-- Notes - might want different background colours within the 
-- same grid...
--
-- Can achieve this with a filled square drawn at the back.
--