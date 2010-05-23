{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapesPic where

import Wumpus.Core
import Wumpus.Extra ( ixLeftRightDown )
import Wumpus.Extra.PictureLanguage hiding ( center, Composite )
import Wumpus.Extra.SafeFonts
import Wumpus.Extra.Shape
import Wumpus.Extra.SVGColours


import Data.Monoid
import System.Directory


-- Note 
-- Processing has (0,0) at top left, 
-- PostScript has (0,0) at bottom left
-- hence the @scale 1 (-1)@

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/"
    sequence_ [ test01, test02, test03 ]



--------------------------------------------------------------------------------
-- 

test01 :: IO ()
test01 = do 
   writeEPS_latin1 "./out/Shapes1.eps" picture1
   writeSVG_latin1 "./out/Shapes1.svg" picture1



picture1 :: Picture Double
picture1 = rect1 ->- rect2 ->- rect3 ->- blabel
  where
    rect1 = frameComposite $ strokeRectangle () $ r1
    rect2 = coorda r2
    rect3 = coorda r1
    r1    = rectangle 160 50 (P2 50 25) `addLabel` "Rectangle!"
    r2    = rotate45About (center r1) r1

    blabel = illustrateBoundsPrim red $ textlabel () "Rectangle!" zeroPt

coorda :: (Floating u, Fractional u , Ord u) => Rectangle u -> Picture u
coorda rect = frameComposite $ mconcat
    [ strokeRectangle () rect
    , drawCoordinate red    $ coordinate (center rect)
    , drawCoordinate green  $ coordinate (north rect)
    , drawCoordinate blue   $ coordinate (south rect)
    , drawCoordinate brown  $ coordinate (east rect)
    , drawCoordinate salmon $ coordinate (west rect)
    , drawCoordinate indigo $ coordinate (northeast rect)
    , drawCoordinate indigo $ coordinate (southeast rect)
    , drawCoordinate indigo $ coordinate (southwest rect)
    , drawCoordinate indigo $ coordinate (northwest rect)
    ] 

--------------------------------------------------------------------------------

test02 :: IO ()
test02 = do 
   writeEPS_latin1 "./out/Shapes2.eps" picture2
   writeSVG_latin1 "./out/Shapes2.svg" picture2


picture2 :: DPicture
picture2 = rect1 ->- text1 ->- circ1 ->- circ2
  where
    rect1 = drawWithAnchors (strokeRectangle red) (rectangle 80 40 zeroPt) 
    text1 = drawWithAnchors (drawFreeLabel blue)   
                            (freeLabel courier36 "Wumpus!" zeroPt)
    circ1 = drawWithAnchors (strokeCircle red) (rotate30 c1)
    circ2 = drawWithAnchors (strokeCircle red) (scale 2 4 c1)

    c1 = circle 20 zeroPt `addLabel` "20"

drawWithAnchors :: (Floating u, Ord u, AnchorCenter t, AnchorCardinal t
                   , u ~ DUnit t) 
                => (t -> Composite u) -> t -> Picture u
drawWithAnchors primf t = frameComposite $ mconcat $ primf t : xs
  where
    xs = map (drawCoordinate indigo) $ cardinals t

cardinals :: (Num u, AnchorCenter t, AnchorCardinal t
             , u ~ DUnit t) 
          => t -> [Coordinate u]
cardinals = map coordinate . sequence funs
  where
    funs = [center, north, northeast, east, southeast, south
                         , southwest, west, northwest ]



--------------------------------------------------------------------------------

test03 :: IO ()
test03 = do 
   writeEPS_latin1 "./out/Shapes3.eps" picture3
   writeSVG_latin1 "./out/Shapes3.svg" picture3

mkCoordinate :: Fractional u => Point2 u -> Composite u
mkCoordinate pt = drawCoordinate lightSteelBlue $ coordinate pt

mkCircle :: (Floating u, Ord u) => Point2 u -> Composite u
mkCircle pt = fillCircle lightSteelBlue $ circle 60 pt `addLabel` "Circle"

mkDiamond :: (Fractional u, Ord u) => Point2 u -> Composite u
mkDiamond pt = fillDiamond lightSteelBlue $ diamond 120 120  pt `addLabel` "Diamond"

mkLabel :: Fractional u => Point2 u -> Composite u
mkLabel =  drawFreeLabel black . freeLabel wumpus_default_font "FreeLabel" 

mkRectangle :: (Fractional u, Ord u) => Point2 u -> Composite u
mkRectangle pt = 
    fillRectangle lightSteelBlue $ rectangle 140 40 pt `addLabel` "Rectangle" 

picture3 :: DPicture
picture3 = frameComposite $ mconcat $ zipWith ($) picfs ixs
  where
    picfs = [ mkCoordinate, mkCircle, mkDiamond, mkLabel, mkRectangle ]

ixs :: Num u => [Point2 u]
ixs = ixLeftRightDown 3 5 (fmap (*200)) 
