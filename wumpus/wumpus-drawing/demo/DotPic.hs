{-# OPTIONS -Wall #-}

module DotPic where


import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Text.SafeFonts

import FontLoaderUtils

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import System.Directory



main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPicture  $ mb_gs
    maybe afm_failk makeAfmPicture $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do 
    putStrLn "Using GhostScript metrics..."
    (base_metrics, msgs) <- loadGSMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) dot_pic
    writeEPS "./out/dots01_gs.eps" pic1
    writeSVG "./out/dots01_gs.svg" pic1

 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    (base_metrics, msgs) <- loadAfmMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) dot_pic
    writeEPS "./out/dots01_afm.eps" pic1
    writeSVG "./out/dots01_afm.svg" pic1

 
makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fillColour peru . fontFace helvetica . metricsContext 24


dot_pic :: CtxPicture Double
dot_pic = drawTracing $ tableGraphic $ 
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
    , dotTriangle
    ]


tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => [DotLocImage u] -> TraceDrawing u ()
tableGraphic imgs = zipWithM_ makeDotDrawing imgs ps
  where
    ps = unchain (coordinateScalingContext 1 36) $ tableDown (length imgs) 1



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
