{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

-- Acknowledgment - the petri net is taken from Claus Reinke\'s
-- paper /Haskell-Coloured Petri Nets/.


module PetriNet where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Paths
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived
import Wumpus.Basic.SafeFonts

import Wumpus.Core                              -- package: wumpus-core


import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/petri_net.eps" petri_net
    writeSVG_latin1 "./out/petri_net.svg" petri_net
    


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
              connector' (east pw)  (west tu1)              
              connector' (east tu1) (west rtw)
              connector' (east rtw) (west tu2)
              connector' (east tu2) (west w)
              connector' (east w)   (west tu3)
              connectorC 32 (north tu3) (north pw)
              connector' (east pr)  (west tl1)              
              connector' (east tl1) (west rtr)
              connector' (east rtr) (west tl2)
              connector' (east tl2) (west r)
              connector' (east r)   (west tl3)
              connectorC (-32) (south tl3) (south pr)
              connector' (southwest res) (northeast tl2)
              connector' (northwest tl3) (southeast res)
              connectorD 6    (southwest tu3) (northeast res)
              connectorD (-6) (southwest tu3) (northeast res) 
              connectorD 6    (northwest res) (southeast tu2)
              connectorD (-6) (northwest res) (southeast tu2) 
              draw $ lblParensParens `at` (P2 (-36) 150)
              draw $ lblParensParens `at` (P2 300 60)
              draw $ lblParensParensParens `at` (P2 (-52) (-14))
              draw $ lblBold "processing_w"   `at` (southwards 12 pw)
              draw $ lblBold "ready_to_write" `at` (southwards 12 rtw)
              draw $ lblBold "writing"        `at` (southwards 12 w)
              draw $ lblBold' "resource"      `at` (P2 300 72)
              draw $ lblBold "processing_r"   `at` (northwards 12 pr)
              draw $ lblBold "ready_to_read"  `at` (northwards 12 rtr)
              draw $ lblBold "reading"        `at` (northwards 12 r)
              return ()

greenFill :: DrawingCtxM m => m a -> m a
greenFill = localize (fillColour lime_green)

place :: (Real u, Floating u, DrawingCtxM m, TraceM m, u ~ MonUnit m) 
      => u -> u -> m (Circle u)
place x y = greenFill $ drawi $ borderedShape $ circle 14 $ P2 x y

transition :: (Real u, Floating u, DrawingCtxM m, TraceM m, u ~ MonUnit m) 
           => u -> u -> m (Rectangle u)
transition x y = 
    greenFill $ drawi $ borderedShape $ rectangle 32 22 $ P2 x y




connector' :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
         , Real u, Floating u, FromPtSize u ) 
      => Point2 u -> Point2 u -> m ()
connector' p0 p1 = 
    drawi_ $ situ2 (strokeConnector (rightArrow connLine tri45)) p0 p1


connectorC :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
           => u -> Point2 u -> Point2 u -> m ()
connectorC v p0 p1 = 
    drawi_ $ situ2 (strokeConnector (rightArrow (connRightVHV v) tri45)) p0 p1

connectorD :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
           => u -> Point2 u -> Point2 u -> m ()
connectorD u p0 p1 = 
    drawi_ $ situ2 (strokeConnector (rightArrow (connIsosceles u) tri45)) p0 p1


lblParensParens :: Num u => LocGraphic u
lblParensParens = localize (fontface helvetica) $ textline "(),()"

lblParensParensParens :: Num u => LocGraphic u
lblParensParensParens = localize (fontface helvetica) $ textline "(),(),()"


lblBold' :: Num u => String -> LocGraphic u
lblBold' ss = localize (fontface helvetica_bold) $ textline ss


lblBold :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
lblBold ss = localize (fontface helvetica_bold) $ centermonoTextline ss