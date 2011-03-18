{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


-- Acknowledgment - the Arrow diagram is taken from Ross 
-- Paterson\'s slides /Arrows and Computation/.

-- NOTE - this example now rather out-of-date. Wumpus-Drawing has 
-- some new features to make node/connector drawing a bit easier.

module ArrowCircuit where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Paths 
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

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
    base_metrics <- loadGSFontMetrics font_dir ["Times-Roman", "Times-Italic"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) circuit_pic
    writeEPS "./out/arrow_circuit01.eps" pic1
    writeSVG "./out/arrow_circuit01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Times-Roman", "Times-Italic"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) circuit_pic
    writeEPS "./out/arrow_circuit02.eps" pic1
    writeSVG "./out/arrow_circuit02.svg" pic1 


 
makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font times_roman . metricsContext 11



-- Note - quite a bit of this diagram was produced /by eye/, 
-- rather than using anchors directly - e.g. the placing of the 
-- ptext labels and the anchors displaced by vectors.
--

-- Note `at` currently does not work for Shapes.
         
circuit_pic :: CtxPicture
circuit_pic = drawTracing body 

body :: TraceDrawing Double ()
body = do
    a1 <- drawi $ rrectangle 12 66 30 `at` P2 0 72
    atext a1 "CONST 0"
    a2 <- drawi $ (strokedShape $ circle 16) `at` P2 120 60
    atext a2 "IF"
    a3 <- drawi $ (strokedShape $ circle 16) `at` P2 240 28
    atext a3 "+1"
    a4 <- drawi $ (strokedShape $ rectangle 66 30) `at` P2 120 0
    atext a4 "DELAY 0"
    connWith connLine (east a1) (fmap (.+^ hvec 76) $ east a1)
    connWith connLine (east a2) (fmap (.+^ hvec 180) $ east a2)
    connWith connLine (fmap (.+^ vvec 40) $ north a2) (north a2)
    connWith connLine (fmap (.+^ vvec 16) $ north a3) (north a3)  
    connWith connRightVH  (south a3) (east a4)
    connWith (connRightHVH (-30)) (west a4)  (southwest a2)
    ptext (P2  40  10) "next"
    ptext (P2 152 100) "reset"
    ptext (P2 252  72) "output"
    return ()


aconnect :: ConnectorQuery u a -> Anchor u -> Anchor u -> Query a
aconnect conn a0 a1 = a0 >>= \p0 -> a1 >>= \p1 -> connect conn p0 p1



connWith :: ( TraceM m, DrawingCtxM m, u ~ MonUnit (m ())
            , Real u, Floating u, InterpretUnit u ) 
         => PathQuery u -> Anchor u -> Anchor u -> m ()
connWith con a0 a1 = localize double_point_size $ 
    drawi_ $ aconnect (rightArrow tri45 con) a0 a1


atext :: ( CenterAnchor t u
         , Real u, Floating u, InterpretUnit u
         , TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) )
      => t u -> String -> m ()
atext ancr ss = 
   drawi_ $ center ancr >>= \pt -> textAlignCenter ss `at` pt


ptext :: ( Real u, Floating u, InterpretUnit u
         , TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) )
      => Point2 u -> String -> m ()
ptext pt ss = localize (font_attr times_italic 14) $ 
    drawi_ $ textAlignCenter ss `at` pt


-- Note - return type is a LocImage not a shape...
--
rrectangle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => Double -> u -> u -> LocImage Rectangle u
rrectangle r w h = 
    localize (round_corner_factor r) $ strokedShape (rectangle w h)
