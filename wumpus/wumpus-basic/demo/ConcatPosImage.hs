{-# OPTIONS -Wall #-}


-- Demo / test the various PoisGgraphic concatenate functions.

module ConcatPosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    mapM_ mf all_concats
  where
    mf (name, pair) = do
      let pic1 = runCtxPictureU std_attr (drawPic pair)
      writeEPS ("./out/pg_" ++ name ++ "01.eps") pic1
      writeSVG ("./out/pg_" ++ name ++ "01.svg") pic1


all_concats :: [ (String, (DPosGraphic, DPosGraphic)) ]
all_concats = 
    [ ("plus",      ( tall_rect `hplus` long_rect
                    , tall_rect `vplus` long_rect ))
    , ("space",     ( hspace 5 tall_rect long_rect
                    , hspace 5 tall_rect long_rect ))
    , ("cat",       ( hcat [mini_rect, tall_rect, long_rect]
                    , vcat [mini_rect, tall_rect, long_rect] ))
    , ("sep",       ( hsep 5 [mini_rect, tall_rect, long_rect]
                    , vsep 5 [mini_rect, tall_rect, long_rect] ))
    ]


std_attr :: DrawingContext
std_attr = standardContext 24


drawPic :: (DPosGraphic, DPosGraphic) -> DCtxPicture
drawPic = drawTracing . localize (fillColour red) . picBody 


picBody :: (Floating u, Ord u, FromPtSize u) 
        => (PosGraphic u, PosGraphic u) ->  TraceDrawing u ()
picBody (f,g) = do
    draw $ testDraw f CENTER `at` (P2   0 350)
    draw $ testDraw f NN     `at` (P2 100 350)
    draw $ testDraw f SS     `at` (P2 200 350)
    draw $ testDraw f EE     `at` (P2 300 350)
    draw $ testDraw f WW     `at` (P2 400 350)
    draw $ testDraw f NE     `at` (P2 100 250)
    draw $ testDraw f SE     `at` (P2 200 250)
    draw $ testDraw f SW     `at` (P2 300 250)
    draw $ testDraw f NW     `at` (P2 400 250)
    draw $ testDraw g CENTER `at` (P2   0 100)
    draw $ testDraw g NN     `at` (P2 100 100)
    draw $ testDraw g SS     `at` (P2 200 100)
    draw $ testDraw g EE     `at` (P2 300 100)
    draw $ testDraw g WW     `at` (P2 400 100)
    draw $ testDraw g NE     `at` (P2 100 0)
    draw $ testDraw g SE     `at` (P2 200 0)
    draw $ testDraw g SW     `at` (P2 300 0)
    draw $ testDraw g NW     `at` (P2 400 0)
    return ()    

testDraw :: Floating u => PosGraphic u -> RectPosition -> LocGraphic u
testDraw pgf rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = let pg = startPosition rpos pgf
          in promoteR1 $ \pt-> 
              (pg `at` pt) >>= \(r,g0) -> drawRect r `oplus` (return (uNil,g0))
          


drawRect :: Fractional u => BorderRect u -> Graphic u
drawRect (BorderRect bl w h) = 
    localize (strokeColour blue) $ strokedRectangle w h `at` bl


tall_rect :: Floating u => PosGraphic u 
tall_rect = makePosGraphic opos (mkRectBl w h)
  where
    w    = 10
    h    = 30
    opos = ObjectPos { op_x_minor = 0
                     , op_x_major = w
                     , op_y_minor = 0
                     , op_y_major = h }
 

-- start-point - bottom left
mkRectBl :: Floating u => u -> u -> LocGraphic u
mkRectBl w h = promoteR1 $ \bl -> 
    let br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in closedStroke $ vertexPath [bl, br, tr, tl]




long_rect :: Floating u => PosGraphic u 
long_rect = makePosGraphic opos (mkRectMinor m w h)
  where
    m    = 10
    w    = 40 
    h    = 15
    opos = ObjectPos { op_x_minor = m
                     , op_x_major = (w-m)
                     , op_y_minor = m
                     , op_y_major = (h-m) }
 

-- start-point - +10 +10
mkRectMinor :: Floating u => u -> u -> u -> LocGraphic u
mkRectMinor m w h = promoteR1 $ \pt -> 
    let bl = displaceVec (vec (-m) (-m)) pt
        br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in closedStroke $ vertexPath [bl, br, tr, tl]




mini_rect :: Floating u => PosGraphic u 
mini_rect = makePosGraphic opos (mkRectCenter w h)
  where
    w    = 16
    h    = 16
    opos = ObjectPos { op_x_minor = 0.5 * w
                     , op_x_major = 0.5 * w
                     , op_y_minor = 0.5 * h
                     , op_y_major = 0.5 * h }
 

-- start-point - +10 +10
mkRectCenter :: Floating u => u -> u -> LocGraphic u
mkRectCenter w h = promoteR1 $ \pt -> 
    let hw = 0.5 * w
        hh = 0.5 * h
        bl = displaceVec (vec (-hw) (-hh)) pt
        br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in closedStroke $ vertexPath [bl, br, tr, tl]

