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
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived
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
    pconnector upper2 upper1 (timesGraphic "h")
    pconnector upper2 upper3 (lrtextGraphic $ alpha >> escName "multiply" 
                                                    >> char 'I')
    pconnector upper3 upper4 (lrtextGraphic $ char 'g' >> escName "onesuperior")
    pconnector lower1 lower0 (timesGraphic "f")
    pconnector lower2 lower1 (symbolGraphic " ")
    pconnector lower3 lower2 (timesGraphic " ")
    pconnector lower0 upper1 (symbolGraphic "&#phi1;")
    pconnector lower1 upper1 (timesGraphic " ")
    pconnector upper2 lower2 (timesGraphic "g")
    pconnector upper2 lower3 (symbolGraphic " ")
    pconnector upper3 lower3 (symbolGraphic " ")
    pconnector upper3 lower4 (symbolGraphic " ")
    pconnector upper4 lower4 (symbolGraphic " ")
    return ()


italiclabel :: ( Real u, Floating u, FromPtSize u
               , DrawingCtxM m, TraceM m, u ~ MonUnit m ) 
            => u -> u -> String -> m (FreeLabel u)
italiclabel x y ss = localCtx (fontface timesItalic)
                              (drawi $ drawShape $ translate x y $ freelabel ss)


symbolGraphic :: Num u => String -> LocGraphic u
symbolGraphic ss = localLG (fontface symbol) (textline ss)

timesGraphic :: Num u => String -> LocGraphic u
timesGraphic ss = localLG (fontface timesItalic) (textline ss)


lrtextGraphic :: (Num u, FromPtSize u) 
              => LRText u a -> LocGraphic u
lrtextGraphic ma = localLG (fontface timesItalic) (execLRText ma)





connector :: ( Real u, Floating u, FromPtSize u
             , DrawingCtxM m, TraceM m, u ~ MonUnit m )
          => Point2 u -> Point2 u -> m (Point2 u)
connector p1 p2 = localCtx thin $ do
   p <- drawi $ arrowBarb60 connectS `conn` p1 $ p2
   return (midpoint p)

pconnector :: ( Real u, Floating u, FromPtSize u
              , DrawingCtxM m, TraceM m, u ~ MonUnit m )
           => FreeLabel u -> FreeLabel u -> LocGraphic u -> m ()
pconnector a b gf = 
    (uncurry connector $ radialConnectorPoints a b) >>= \pt -> draw $ gf pt
