{-# OPTIONS -Wall #-}

module DotPic where


import Wumpus.Basic.Chains
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots.AnchorDots
import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_ctx dot_drawing
    writeEPS "./out/dots01.eps" pic1
    writeSVG "./out/dots01.svg" pic1
 

dot_drawing :: Drawing Double
dot_drawing = drawTracing $ tableGraphic $ 
    [ dotHLine
    , dotVLine
    , dotX
    , dotPlus
    , dotCross
    , dotDiamond
    , dotDisk
    , dotSquare
    , dotCircle
    , dotPentagon
    , dotStar
    , dotAsterisk
    , dotOPlus
    , dotOCross
    , dotFOCross
    , dotFDiamond
    , dotText "%" 
    ]


tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => [DotLocImage u] -> TraceDrawing u ()
tableGraphic imgs = zipWithM_ makeDotDrawing imgs ps
  where
    ps = unchain (coordinateScalingContext 1 36) $ tableDown (length imgs) 1


 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 24



makeDotDrawing :: (Real u, Floating u, FromPtSize u) 
               => DotLocImage u -> Point2 u -> TraceDrawing u ()
makeDotDrawing dotF pt = do 
    dashline
    mapM_ (\v -> drawi $ dotF `at` pt .+^ v) displacements
  where
    all_pts  = map (pt .+^) displacements
    dashline = localize attrUpd (draw $ openStroke $ vertexPath all_pts)

    attrUpd  :: DrawingContext -> DrawingContext
    attrUpd  =  dashPattern (evenDashes 1) . strokeColour cadet_blue


displacements :: Num u => [Vec2 u]
displacements = [V2 0 0, V2 64 20, V2 128 0, V2 192 20]


-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs

