{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}
 
module Oscil where

import FontLoaderUtils

import Wumpus.Block.Base
import Wumpus.Block.Csound

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours hiding ( linen )
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.LRText
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
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx gs_metrics) shape_pic
    writeEPS "./out/oscil1_gs.eps" pic1
    writeSVG "./out/oscil1_gs.svg" pic1


makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) shape_pic
    writeEPS "./out/oscil1_afm.eps" pic2
    writeSVG "./out/oscil1_afm.svg" pic2

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 18


shape_pic :: DCtxPicture
shape_pic = drawTracing $ do
    a1  <- drawi $ oscil `at` P2 300 150
    a2  <- drawi $ terminal `at` P2 300 90
    connector (outport a1) (inport1 a2)

    b1  <- drawi $ linen `at` P2 200 150
    b2  <- drawi $ oscil `at` P2 220 74 
    b3  <- drawi $ terminal `at` P2 220 14
    connector (south b1) (northwest $ getOscil b2)
    connector (outport b2) (inport1 b3)

    c1  <- drawi $ expon      `at` P2 100 200
    c2  <- drawi $ adder      `at` P2 100 146
    c3  <- drawi $ buzz       `at` P2  92  92
    c4  <- drawi $ multiplier `at` P2  16  48

    connector (south c1) (north c2)
    connector (south c2) (radialAnchor (d2r (68::Double)) $ getBuzz c3)

    return ()

{-
-- Fontsize 18 is good for labels eg \"OSCIL\".
-- Parameters, inputs and outputs should be smaller.
--
shapeSty :: DrawingContextF
shapeSty = strokeColour black . thick . fontSize 18 . fontFace helvetica

-- Maybe - Csound specific objects should be newtypes over shapes 
-- that have @input@ and @output@ ports rather than anchors?

terminal :: (Real u, Floating u, FromPtSize u) 
         => LocImage u (Circle u)
terminal = localize shapeSty $ strokedShape $ circle 6


linen :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Trapezium u)
linen = localize shapeSty $ strokedShape $ body 
  where
    body  = setDecoration textF $ trapezium 88 40 ang ang
    textF = ignoreAns (multiAlignCenter CENTER "LINEN")    
    ang   = d2r (72::Double)

-- expon same as linen...
expon :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Trapezium u)
expon = localize shapeSty $ strokedShape $ body 
  where
    body  = setDecoration textF $ trapezium 88 40 ang ang
    textF = ignoreAns (multiAlignCenter CENTER "EXPON")
    ang   = d2r (72::Double)

buzz :: (Real u, Floating u, FromPtSize u) 
     => LocImage u (Rectangle u)
buzz = localize shapeSty $ strokedShape $ body 
  where
    body  = setDecoration textF $ rectangle 88 40
    textF = ignoreAns (multiAlignCenter CENTER "BUZZ")

-- needs \"+\" for body
adder :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Circle u)
adder = localize shapeSty $ strokedShape $ circle 10

-- needs \"x\" for body
multiplier :: (Real u, Floating u, FromPtSize u) 
           => LocImage u (Circle u)
multiplier = localize shapeSty $ strokedShape $ circle 10

-}

connector :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
             , Real u, Floating u, FromPtSize u ) 
          => Point2 u -> Point2 u -> m ()
connector p0 p1 = 
    drawi_ $ apply2R2 (rightArrow tri45 connLine) p0 p1
