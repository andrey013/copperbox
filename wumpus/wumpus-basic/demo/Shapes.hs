{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Paths
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived


import Wumpus.Core                              -- package: wumpus-core


import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/shapes01.eps" pic1
    writeSVG_latin1 "./out/shapes01.svg" pic1
    writeEPS_latin1 "./out/shapes02.eps" petri_net
    writeSVG_latin1 "./out/shapes02.svg" petri_net
    

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 14) $ do
         _ <- drawi $ drawShape $ translate 220 10 $ rotate30
                                                   $ lrectangle 90 30 "Rectangle"
         _ <- drawi $ drawShape $ translate 100  0 $ lcircle 10 "C0"
   
         _ <- localCtx (primaryColour red) $ 
                       drawi $ drawShape $ translate 220 10 $ rotate30 $ coordinate
         _ <- drawi $ drawShape $ translate 0   40 $ ldiamond 10 10 "d1"
         _ <- drawi $ drawShape $ translate 400 50 $ lrectangle 20 100 "R2"
         return ()




petri_net :: DPicture
petri_net = liftToPictureU $ execDrawing (standardContext 14) $ do
              pw     <- place 0 140
              tu1    <- transition 70 140
              rtw    <- place 140 140
              tu2    <- transition 210 140
              w      <- place 280 140
              tu3    <- transition 350 140
              res    <- place 280 70
              pr     <- place 0 0
              tl1    <- transition 70 0
              rtr    <- place 140 0
              tl2    <- transition 210 0
              r      <- place 280 0
              tl3    <- transition 350 0
              connector (east pw)  (west tu1)              
              connector (east tu1) (west rtw)
              connector (east rtw) (west tu2)
              connector (east tu2) (west w)
              connector (east w)   (west tu3)
              connectorC 32 (north tu3) (north pw)

              connector (east pr)  (west tl1)              
              connector (east tl1) (west rtr)
              connector (east rtr) (west tl2)
              connector (east tl2) (west r)
              connector (east r)   (west tl3)
              connectorC (-32) (south tl3) (south pr)
              connector (southwest res) (northeast tl2)
              connector (northwest tl3) (southeast res)
              return ()

greenFill :: DrawingCtxM m => m a -> m a
greenFill = localCtx (secondaryColour lime_green)

place :: (Real u, Floating u, DrawingCtxM m, TraceM m, u ~ MonUnit m) 
      => u -> u -> m (Circle u)
place x y = greenFill $ drawi $ drawShape $ translate x y $ circle 14

transition :: (Real u, Floating u, DrawingCtxM m, TraceM m, u ~ MonUnit m) 
           => u -> u -> m (Rectangle u)
transition x y = greenFill $ drawi $ drawShape $ translate x y $ rectangle 32 22

connector :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
          => Point2 u -> Point2 u -> m ()
connector p1 p2 = do
   _ <- drawi $ arrowTri60 connectS `conn` p1 $ p2
   return ()


connectorC :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
           => u -> Point2 u -> Point2 u -> m ()
connectorC v p1 p2 = do
   _ <- drawi $ arrowTri60 (arbv v) `conn` p1 $ p2
   return ()

