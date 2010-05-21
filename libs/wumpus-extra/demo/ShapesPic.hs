{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapesPic where

import Wumpus.Core
-- import Wumpus.Extra hiding ( rectangle )
import Wumpus.Extra.PictureLanguage hiding ( center )
import Wumpus.Extra.SafeFonts
import Wumpus.Extra.Shape
import Wumpus.Extra.SVGColours

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
picture1 = rect1 ->- rect2 ->- rect3
  where
    rect1 = frame $ strokeRectangle () $ r1
    rect2 = coorda r2
    rect3 = coorda r1
    r1    = rectangle 100 50 (P2 50 25)
    r2    = rotate45About (center r1) r1

coorda :: (Floating u, Fractional u , Ord u) => Rectangle u -> Picture u
coorda rect = frameMulti 
    [ strokeRectangle () $ rect
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
picture2 = rect1 ->- text1
  where
    rect1 = drawWithAnchors (strokeRectangle red) (rectangle 80 40 zeroPt)
    text1 = drawWithAnchors (drawTextLine blue)   
                            (textLine courier36 "Wumpus!" zeroPt)
    

drawWithAnchors :: (Floating u, Ord u, AnchorCenter t, AnchorCardinal t
                   , u ~ DUnit t) 
                => (t -> Primitive u) -> t -> Picture u
drawWithAnchors primf t = frameMulti $ primf t : xs
  where
    xs = map (drawCoordinate indigo) $ cardinals t

cardinals :: (Num u, AnchorCenter t, AnchorCardinal t
             , u ~ DUnit t) 
          => t -> [Coordinate u]
cardinals = map coordinate . sequence funs
  where
    funs = [center, north, northeast, east, southeast, south
                         , southwest, west, northwest ]