{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapesPic where

import Wumpus.Core
-- import Wumpus.Extra hiding ( rectangle )
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
    sequence_ [ test01, test02 ]



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
    text1 = drawWithAnchors (drawTextLine blue)   
                            (textLine courier36 "Wumpus!" zeroPt)
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

picture3 :: DPicture
picture3 = frameComposite $ drawTextLine red $ uniformScale 5 $ rotate45 $ 
             textLine wumpus_default_font "Wumpus!" zeroPt
