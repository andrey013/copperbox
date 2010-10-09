{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

-- Acknowledgment - the diagram is taken from Martin Erwig\'s
-- paper /Random Access to Abstract Data Types/.


module Synchro where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Paths
import Wumpus.Basic.Shapes
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRText
import Wumpus.Basic.Text.LRSymbol

import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/synchro.eps" pic1
    writeSVG_latin1 "./out/synchro.svg" pic1

-- Note - connector could create radial anchors if it took the 
-- vector ctr -> ctr then found its direction.
-- This would save all the cardinals...

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 9) $ do
    upper1     <- italiclabel  66 50 "C"
    upper2     <- italiclabel 154 50 "G(A)&#multiply;B"
    upper3     <- italiclabel 244 50 "H(A)&#multiply;B"
    upper4     <- italiclabel 312 50 "K(B)"
    lower0     <- italiclabel   0   0 "M(C)"
    lower1     <- italiclabel  66   0 "H(A &#plus; C)"
    lower2     <- italiclabel 154   0 "H(A &#plus; G(A) &#multiply; B)"
    lower3     <- italiclabel 244   0 "H(A) &#multiply; J(B)"
    lower4     <- italiclabel 312   0 "J(B)"
    lblconn upper2 upper1 (timesGraphic "h")
    lblconn upper2 upper3 (lrtextGraphic $ alpha >> escName "multiply" 
                                                    >> char 'I')
    lblconn upper3 upper4 (lrtextGraphic $ char 'g' >> escName "onesuperior")
    lblconn lower1 lower0 (timesGraphic "f")
    lblconn lower2 lower1 (symbolGraphic " ")
    lblconn lower3 lower2 (timesGraphic " ")
    lblconn lower0 upper1 (symbolGraphic "&#phi1;")
    lblconn lower1 upper1 (timesGraphic " ")
    lblconn upper2 lower2 (timesGraphic "g")
    lblconn upper2 lower3 (symbolGraphic " ")
    lblconn upper3 lower3 (symbolGraphic " ")
    lblconn upper3 lower4 (symbolGraphic " ")
    lblconn upper4 lower4 (symbolGraphic " ")
    return ()


italiclabel :: ( Real u, Floating u, FromPtSize u
               , DrawingCtxM m, TraceM m, u ~ MonUnit m ) 
            => u -> u -> String -> m (PlaintextAnchor u)
italiclabel x y ss = localize (fontface times_italic)
                              (drawi $ drawText $ translate x y $ plaintext ss)


symbolGraphic :: Num u => String -> LocGraphic u
symbolGraphic ss = localize (fontface symbol) . (textline ss)

timesGraphic :: Num u => String -> LocGraphic u
timesGraphic ss = localize (fontface times_italic) . (textline ss)


lrtextGraphic :: (Num u, FromPtSize u) 
              => LRText u a -> LocGraphic u
lrtextGraphic ma = localize (fontface times_italic) . (execLRText ma)





conn :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
          => Point2 u -> Point2 u -> m (Point2 u)
conn p1 p2 = localize thin $ do
   p <- drawi $ strokeConnector (rightArrow connect barb45) p1 p2
   return (fst $ midpoint p)

lblconn :: ( Real u, Floating u, FromPtSize u
              , DrawingCtxM m, TraceM m, u ~ MonUnit m )
           => PlaintextAnchor u -> PlaintextAnchor u -> LocGraphic u -> m ()
lblconn a b gf = 
    (uncurry conn $ radialConnectorPoints a b) >>= \pt -> draw $ gf pt
