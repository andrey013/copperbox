{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


-- Acknowledgment - the Arrow diagram is taken from Ross 
-- Paterson\'s slides /Arrows and Computation/.


module Arrow01 where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths 
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Shapes

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/arrow01.eps" pic1
    >> writeSVG_latin1 "./out/arrow01.svg" pic1 

times_ctx :: DrawingContext
times_ctx = fontface times_roman $ standardContext 12

         
pic1 :: Picture Double 
pic1 = liftToPictureU $ execDrawing times_ctx $ do
    a1 <- drawi $ strokedShape $ translate 0   72 $ rrectangle 12 66 30
    a2 <- drawi $ strokedShape $ translate 120 60 $ circle 18
    a3 <- drawi $ strokedShape $ translate 240 30 $ circle 18
    a4 <- drawi $ strokedShape $ translate 120  0 $ rectangle 66 30
    straightconn (east a1) (east a1 .+^ hvec 74)
    straightconn (east a2) (east a2 .+^ hvec 180)
    straightconn (north a2 .+^ vvec 40) (north a2)  
    return ()

{-
    drawi_ $ dotDisk `ati` (P2 0 0)
    drawi_ $ dotDisk `ati` (P2 100 0)
    drawi_ $ strokeConnector (rightArrow connect otri60) (P2 0 0) (P2 100 0) 
    drawi_ $ dotText "k" `ati` (P2 120 6)  
    drawi_ $ dotText "k" `ati` (P2 88  (-20))
    drawi_ $ strokeConnector (rightArrow connect rbracket) (P2 130 0) (P2 160 0) 
-}      

-- Note - conn from Basic.Graphic is now questionable...

straightconn :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
                 , Real u, Floating u, FromPtSize u ) 
             => Point2 u -> Point2 u -> m ()
straightconn p0 p1 = localize doublesize $ 
    drawi_ $ strokeConnector (rightArrow connect tri45) p0 p1
