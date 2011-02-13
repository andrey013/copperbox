{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}
 
module Oscil where

import FontLoaderUtils

import Wumpus.Block.Base
import Wumpus.Block.Csound

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript


import Wumpus.Core                              -- package: wumpus-core

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
    metrics <- loadGSFontMetrics font_dir ["Helvetica"]
    printLoadErrors metrics
    let pic1 = runCtxPictureU (makeCtx metrics) shape_pic
    writeEPS "./out/oscil1_gs.eps" pic1
    writeSVG "./out/oscil1_gs.svg" pic1


makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors metrics
    let pic2 = runCtxPictureU (makeCtx metrics) shape_pic
    writeEPS "./out/oscil1_afm.eps" pic2
    writeSVG "./out/oscil1_afm.svg" pic2

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 18


shape_pic :: DCtxPicture
shape_pic = drawTracing $ do
    a1  <- drawi $ oscil "f1" `at` P2 300 150
    a2  <- drawi $ terminal `at` P2 300 90
    connector (outport1 a1) (inport1 a2)
    draw $ labelOutport1 a1 "(a1)"

    b1  <- drawi $ linen `at` P2 200 150
    b2  <- drawi $ zoscil `at` P2 220 74 
    b3  <- drawi $ terminal `at` P2 220 14
    connector (outport1 b1) (inport2a b2)
    connector (outport1 b2) (inport1 b3)

    c1  <- drawi $ expon      `at` P2 100 200
    c2  <- drawi $ adder      `at` P2 100 146
    c3  <- drawi $ buzz       `at` P2  92  92
    c4  <- drawi $ multiplier `at` P2  16  48

    connector (outport1 c1) (north c2)
    connector (south c2) (radialAnchor (d2r (68::Double)) $ getBuzz c3)

    return ()



connector :: ( TraceM m, DrawingCtxM m, u ~ DUnit (m ())
             , Real u, Floating u, FromPtSize u ) 
          => Point2 u -> Point2 u -> m ()
connector p0@(P2 x0 _) p1@(P2 x1 _) 
   | tolEq 1.0 x0 x1 = drawi_ $ apply2R2 (rightArrow tri45 connLine) p0 p1
   | otherwise       = drawi_ $ apply2R2 (rightArrow tri45 connMidH) p0 p1

tolEq :: (Ord u, FromPtSize u) => u -> u -> u -> Bool
tolEq tol a b = abs (a - b) < (tol * fromPtSize 1) 


-- This is a good one for Wumpus-Drawing, however current 
-- connector code needs a re-think.
--

-- | Right-angled connector - go vertical go horizontal go vertical.
--
connMidH :: (Real u, Floating u, FromPtSize u) 
         => PathCF u
connMidH = promoteR2 $ \ sp@(P2 x0 y0) ep@(P2 x1 y1) -> 
    let ymid = y0 + (0.5 * (y1 - y0))
    in roundCornerPath [sp, P2 x0 ymid, P2 x1 ymid, ep]


-- | Build the path with interior round corners.
-- 
roundCornerPath :: (Real u, Floating u, FromPtSize u) 
                => [Point2 u] -> CF (Path u)
roundCornerPath xs = getRoundCornerSize >>= \sz -> 
    if sz == 0 then return (traceLinePoints xs) 
               else return (roundInterior  sz xs)
