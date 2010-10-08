{-# OPTIONS -Wall #-}

module Arrow01 where

import Wumpus.Basic.Dots
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths 

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/arrow01.eps" pic1
    >> writeSVG_latin1 "./out/arrow01.svg" pic1 

         
pic1 :: Picture Double 
pic1 = liftToPictureU $ execDrawing (standardContext 48) $ do
    drawi_ $ dotDisk `ati` (P2 0 0)
    drawi_ $ dotDisk `ati` (P2 100 0)
    drawi_ $ strokeConnector (rightArrow connect otri60) (P2 0 0) (P2 100 0) 
    drawi_ $ dotText "k" `ati` (P2 120 6)  
    drawi_ $ dotText "k" `ati` (P2 88  (-20))
    drawi_ $ strokeConnector (rightArrow connect rbracket) (P2 130 0) (P2 160 0) 
      

