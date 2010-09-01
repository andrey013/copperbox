{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Deprecated.PictureLanguage

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    sequence_  [ demo01, demo02, demo03, demo04, demo05
               , demo06, demo07, demo08, demo09, demo10
               , demo11, demo12, demo13, demo14 ]



square :: DPicture 
square = frame [cstroke () $ vertexPath xs]
  where
    xs = [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPicture
funnyshape = frame [cstroke () $ vertexPath xs]
  where
    xs = [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


demo01 :: IO ()
demo01 = do 
  writePS_latin1  "./out/picture01.ps"    [funnyshape `nextToH` square]
  writeSVG_latin1 "./out/picture01.svg" $ funnyshape `nextToH` square


pic1 :: Picture Double
pic1 = square `nextToH` (funnyshape `nextToH` funnyshape) `nextToH` square

squares :: Picture Double
squares = square `nextToH` square `nextToH` square

demo02 :: IO ()
demo02 = do 
   writePS_latin1  "./out/picture02.ps"  [squares]
   writeSVG_latin1 "./out/picture02.svg" squares 
    

demo03 :: IO ()
demo03 = do 
    writeEPS_latin1 "./out/picture03.eps" p1 
    writeSVG_latin1 "./out/picture03.svg" p1
  where     
    p1 = square `nextToH` (rotate45About (center squares) squares) `nextToH` square


demo04 :: IO ()
demo04 = do 
    writeEPS_latin1 "./out/picture04.eps" p1
    writeSVG_latin1 "./out/picture04.svg" p1
  where
    p1 = square `nextToV` squares
   

demo05 :: IO ()
demo05 = do 
    writeEPS_latin1 "./out/picture05.eps" p1
    writeSVG_latin1 "./out/picture05.svg" p1
  where
    p1 = square `over` (rotate (pi/4) squares)
   

demo06 :: IO ()
demo06 = do 
    writeEPS_latin1 "./out/picture06.eps" p1
    writeSVG_latin1 "./out/picture06.svg" p1
  where
    p1 = square `over` (rotate45 square)


-- Note the move via @at@ is not apparent when SVG file is 
-- viewed with Mozilla or Chrome - check picture7a.svg
-- We only see that the move has /worked/ when we compose
-- with with `over` a square at the origin. 

demo07 :: IO ()
demo07 = do 
    writeEPS_latin1 "./out/picture07.eps" p1
    writeSVG_latin1 "./out/picture07.svg" p1
    writeSVG_latin1 "./out/picture07a.svg" p2
  where
    p1 = square `over` p2
    p2 = (square `atPoint` (P2 100 30))  `centerOver` (rotate45 square)


demo08 :: IO ()
demo08 = do 
    writeEPS_latin1 "./out/picture08.eps" p1
    writeSVG_latin1 "./out/picture08.svg" p1
  where
    p1 = hspace 20 square square

mkFilledSquare :: RGBi -> Double -> DPicture 
mkFilledSquare rgb n = frame 
  [fill rgb $ vertexPath [ P2 0 0, P2 n 0, P2 n n, P2 0 n ]]


demo09 :: IO ()
demo09 = do 
    writeEPS_latin1 "./out/picture09.eps" p1
    writeSVG_latin1 "./out/picture09.svg" p1
  where
    p1 = (alignH HTop s1 s2) `op` s3
    s1 = uniformScale 1.5  $ mkFilledSquare plum 40
    s2 = uniformScale 1.75 $ mkFilledSquare peru 40
    s3 = scale 3 1.5       $ mkFilledSquare black 40
    op = alignH HBottom
 

demo10 :: IO ()
demo10 = do 
    writeEPS_latin1 "./out/picture10.eps" p1
    writeSVG_latin1 "./out/picture10.svg" p1
  where
    p1 = vsepA VRight 5 s1 [s2,s3]
    s1 = uniformScale 1.5  $ mkFilledSquare plum 40
    s2 = uniformScale 1.75 $ mkFilledSquare peru 40
    s3 = scale 3 1.5       $ mkFilledSquare black 40
 


-- Stroked ellipe problem under scaling...
demo11 :: IO ()
demo11 = do 
    writeEPS_latin1 "./out/picture11.eps" pic
    writeSVG_latin1 "./out/picture11.svg" pic
  where
    pic :: Picture Double
    pic = p1 `nextToV` p2
    p1 = scale 6 12 $ frame [ellipse (plum, LineWidth 2) 4 6 zeroPt]
    p2 = scale 6 12 $ frame [ellipse (peru, LineWidth 2) 6 6 zeroPt]


-- Note the movement of the plum square won't be regarded by 
-- Firefox as it crops whitespace automatically.
demo12 :: IO ()
demo12 = do 
    writeEPS_latin1 "./out/picture12.eps" pic
    writeSVG_latin1 "./out/picture12.svg" pic
  where
    pic :: Picture Double
    pic = p1 `nextToV` p2 `nextToV` p3
    p1 = small_black `centerOver` large_plum     -- moves black
    p2 = large_plum  `centerOver` small_black    -- moves plum
    p3 = small_black `nextToH` large_plum        -- moves plum

    small_black = mkFilledSquare black 10 `atPoint` P2 30 0
    large_plum  = mkFilledSquare plum  40 `atPoint` P2 100 0


demo13 :: IO ()
demo13 = do 
    writeEPS_latin1 "./out/picture13.eps" pic
    writeSVG_latin1 "./out/picture13.svg" pic
  where
    pic :: Picture Double
    pic = (p1 `atPoint` P2 20 20) `nextToH` (p2 `atPoint` P2 60 20) 

    p1 = small_black `nextToV` small_peru
    p2 = small_black `nextToV` small_plum

    small_black = mkFilledSquare black 10 `atPoint` P2 50 0
    small_plum  = mkFilledSquare plum  10 `atPoint` P2 50 0
    small_peru  = mkFilledSquare peru  10 `atPoint` P2 50 0

demo14 :: IO ()
demo14 = do 
    writeEPS_latin1 "./out/picture14.eps" pic
    writeSVG_latin1 "./out/picture14.svg" pic
  where
    pic :: Picture Double
    pic = hsep 40 p1 [p2,p3,p4,p5,p6]

    p1 = alignH HTop    small_black mid_peru
    p2 = alignH HBottom small_black mid_plum
    p3 = alignH HCenter small_black mid_peru

    p4 = alignV VLeft   mid_black small_peru
    p5 = alignV VRight  mid_black small_plum
    p6 = alignV VCenter mid_black small_peru

    small_black = mkFilledSquare black 10 `atPoint` P2 10 0
    mid_plum    = mkFilledSquare plum  25 `atPoint` P2 50 0
    mid_peru    = mkFilledSquare peru  25 `atPoint` P2 50 0

    mid_black   = mkFilledSquare black 25 `atPoint` P2 10 10
    small_plum  = mkFilledSquare plum  10 `atPoint` P2 10 50
    small_peru  = mkFilledSquare peru  10 `atPoint` P2 10 50


