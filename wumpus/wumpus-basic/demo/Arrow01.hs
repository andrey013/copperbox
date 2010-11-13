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
    >> writeEPS "./out/arrow01.eps" pic1
    >> writeSVG "./out/arrow01.svg" pic1 

times_ctx :: DrawingContext
times_ctx = fontface times_roman $ standardContext 11

-- Note - quite a bit of this diagram was produced /by eye/, 
-- rather than using anchors directly - e.g. the placing of the 
-- ptext labels and the anchors displaced by vectors.
--
         
pic1 :: Picture Double 
pic1 = liftToPictureU $ execTraceDrawing times_ctx $ do
    a1 <- drawi $ strokedShape $ rrectangle 12 66 30 $ P2 0 72
    atext a1 "CONST 0"
    a2 <- drawi $ strokedShape $ circle 16 $ P2 120 60
    atext a2 "IF"
    a3 <- drawi $ strokedShape $ circle 16 $ P2 240 28
    atext a3 "+1"
    a4 <- drawi $ strokedShape $ rectangle 66 30 $ P2 120 0
    atext a4 "DELAY 0"
    connWith connLine (east a1) (east a1 .+^ hvec 76)
    connWith connLine (east a2) (east a2 .+^ hvec 180)
    connWith connLine (north a2 .+^ vvec 40) (north a2)
    connWith connLine (north a3 .+^ vvec 16) (north a3)  
    connWith connRightVH  (south a3) (east a4)
    connWith (connRightHVH (-30)) (west a4)  (southwest a2)
    ptext (P2  40  10) "next"
    ptext (P2 152 100) "reset"
    ptext (P2 252  72) "output"
    return ()



connWith :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
            , Real u, Floating u, FromPtSize u ) 
         => ConnectorPath u -> Point2 u -> Point2 u -> m ()
connWith conn p0 p1 = localize doublesize $ 
    drawi_ $ situ2 (strokeConnector (rightArrow conn tri45)) p0 p1


atext :: ( CenterAnchor t, DUnit t ~ u
         , Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => t -> String -> m ()
atext ancr ss = let pt = center ancr in
   drawi_ $ drawText $ plaintext ss $ pt


ptext :: ( Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => Point2 u -> String -> m ()
ptext pt ss = localize (fontsize 14 . fontface times_italic) $ 
    drawi_ $ drawText $ plaintext ss $ pt