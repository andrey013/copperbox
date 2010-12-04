{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


-- Acknowledgment - the Arrow diagram is taken from Ross 
-- Paterson\'s slides /Arrows and Computation/.


module ArrowCircuit where

import Wumpus.Basic.Graphic
import Wumpus.Basic.System.AfmLoader
import Wumpus.Basic.System.GSLoader
import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Paths 
import Wumpus.Drawing.SafeFonts
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.LRText

import Wumpus.Core                      -- package: wumpus-core

import FontLoaderUtils


import Data.AffineSpace

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
    base_metrics <- loadGSMetrics font_dir ["Times-Roman", "Times-Italic"]
    let pic1 = runDrawingU (makeCtx base_metrics) circuit_drawing
    writeEPS "./out/arrow_circuit01.eps" pic1
    writeSVG "./out/arrow_circuit01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmMetrics font_dir ["Times-Roman", "Times-Italic"]
    let pic1 = runDrawingU (makeCtx base_metrics) circuit_drawing
    writeEPS "./out/arrow_circuit02.eps" pic1
    writeSVG "./out/arrow_circuit02.svg" pic1 

 
makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = fontFace times_roman . metricsContext 11



-- Note - quite a bit of this diagram was produced /by eye/, 
-- rather than using anchors directly - e.g. the placing of the 
-- ptext labels and the anchors displaced by vectors.
--

-- Note `at` currently does not work for Shapes.
         
circuit_drawing :: Drawing Double 
circuit_drawing = drawTracing $ do
    a1 <- drawi $ strokedShape $ rrectangle 12 66 30 $ P2 0 72
    atext a1 "CONST 0"
    a2 <- drawi $ strokedShape $ circle 16 $ P2 120 60
    atext a2 "IF"
    a3 <- drawi $ strokedShape $ circle 16 $ P2 240 28
    atext a3 "+1"
    a4 <- drawi $ strokedShape $ rectangle 66 30 $ P2 120 0
    atext a4 "DELAY 0"
    connWith connLine (east a1) (east a1 .+^ hvec 76)
    connWith connLine (east a2) (east a2 .+^ hvec 180)
    connWith connLine (north a2 .+^ vvec 40) (north a2)
    connWith connLine (north a3 .+^ vvec 16) (north a3)  
    connWith connRightVH  (south a3) (east a4)
    connWith (connRightHVH (-30)) (west a4)  (southwest a2)
    ptext (P2  40  10) "next"
    ptext (P2 152 100) "reset"
    ptext (P2 252  72) "output"
    return ()



connWith :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
            , Real u, Floating u, FromPtSize u ) 
         => ConnectorPath u -> Point2 u -> Point2 u -> m ()
connWith con p0 p1 = localize doublesize $ 
    drawi_ $ situ2 (strokeConnector (rightArrow con tri45)) p0 p1


atext :: ( CenterAnchor t, DUnit t ~ u
         , Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => t -> String -> m ()
atext ancr ss = let pt = center ancr in
   drawi_ $ singleLineCC ss `at` pt


ptext :: ( Real u, Floating u, FromPtSize u
         , TraceM m, DrawingCtxM m, u ~ MonUnit m )
      => Point2 u -> String -> m ()
ptext pt ss = localize (fontAttr times_italic 14) $ 
    drawi_ $ singleLineCC ss `at` pt