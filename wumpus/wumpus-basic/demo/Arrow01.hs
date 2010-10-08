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
times_ctx = fontface times_roman $ standardContext 11

-- Note - quite a bit of this diagram was produced /by eye/, 
-- rather than using anchors directly - e.g. the placing of the 
-- ptext labels and the anchors displaced by vectors.
--
         
pic1 :: Picture Double 
pic1 = liftToPictureU $ execDrawing times_ctx $ do
    a1 <- drawi $ strokedShape $ translate 0   72 $ rrectangle 12 66 30
    atext a1 "CONST 0"
    a2 <- drawi $ strokedShape $ translate 120 60 $ circle 16
    atext a2 "IF"
    a3 <- drawi $ strokedShape $ translate 240 28 $ circle 16
    atext a3 "+1"
    a4 <- drawi $ strokedShape $ translate 120  0 $ rectangle 66 30
    atext a4 "DELAY 0"
    connWith connect (east a1) (east a1 .+^ hvec 76)
    connWith connect (east a2) (east a2 .+^ hvec 180)
    connWith connect (north a2 .+^ vvec 40) (north a2)
    connWith connect (north a3 .+^ vvec 16) (north a3)  
    connWith vhconn  (south a3) (east a4)
    connWith (hvhconn (-30)) (west a4)  (southwest a2)
    ptext (P2  40  10) "next"
    ptext (P2 152 100) "reset"
    ptext (P2 252  72) "output"
    return ()


-- Note - conn from Basic.Graphic is now questionable...

connWith :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
            , Real u, Floating u, FromPtSize u ) 
         => ConnectorPath u -> Point2 u -> Point2 u -> m ()
connWith con p0 p1 = localize doublesize $ 
    drawi_ $ strokeConnector (rightArrow con tri45) p0 p1


atext :: ( CenterAnchor t, DUnit t ~ u
         , Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => t -> String -> m ()
atext ancr ss = let (P2 x y) = center ancr in
   drawi_ $ drawText $ translate x y $ plaintext ss


ptext :: ( Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => Point2 u -> String -> m ()
ptext (P2 x y) ss = localize (fontsize 14 . fontface times_italic) $ 
    drawi_ $ drawText $ translate x y $ plaintext ss