{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}



module FeatureModel where

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
    (base_metrics, msgs) <- loadGSMetrics font_dir ["Courier-Bold"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) feature_model 
    writeEPS "./out/feature_model01.eps" pic1
    writeSVG "./out/feature_model01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    (base_metrics, msgs) <- loadAfmMetrics font_dir ["Courier-Bold"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) feature_model 
    writeEPS "./out/feature_model02.eps" pic1
    writeSVG "./out/feature_model02.svg" pic1 

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier_bold . metricsContext 18


-- Note - I haven't worked out how to do @alternative@, @or@ and
-- @repetitions@ yet.
--
         
feature_model :: CtxPicture Double 
feature_model = drawTracing $ do
    lea <- widebox "e" $ P2 150 160    
    lra <- widebox "r" $ P2  60  80
    lsa <- widebox "s" $ P2 240  80
    cmandatory_ lea lra
    cmandatory_ lea lsa

    uGa <- box "G" $ P2   0 0
    uHa <- box "H" $ P2  60 0
    uIa <- box "I" $ P2 120 0

    uAa <- box "A" $ P2 180 0
    uBa <- box "B" $ P2 240 0
    uCa <- box "C" $ P2 300 0

    cmandatory_ lra uGa
    cmandatory_ lra uHa
    cmandatory_ lra uIa

    cmandatory_ lsa uAa
    coptional_  lsa uBa
    cmandatory_ lsa uCa

    return ()


type Box u = Rectangle u


-- Note - ctrCenterLine does not seem to be working well...

makeBox :: (Real u, Floating u, FromPtSize u) 
        => u -> String -> Point2 u -> TraceDrawing u (Box u)
makeBox w ss pt = do 
    a <- drawi $ (strokedShape $ rectangle w 20) `at` pt
    drawi_ $ textAlignCenter ss `at` center a
    -- draw  $ filledDisk 2 `at` center a
    return a

box :: (Real u, Floating u, FromPtSize u) 
    => String -> Point2 u -> TraceDrawing u (Box u)
box = makeBox 40

widebox :: (Real u, Floating u, FromPtSize u) 
        => String -> Point2 u -> TraceDrawing u (Box u)
widebox = makeBox 60


connWith :: ( Real u, Floating u, FromPtSize u ) 
         => Arrowhead u -> Box u -> Box u -> TraceDrawing u (Path u)
connWith arrh b0 b1 = do
   lw <- getLineWidth
   let p0 = south b0
   let p1 = projectAnchor north (realToFrac lw) b1
   drawi $ apply2R2 (rightArrow arrh connLine) p0 p1

infixr 4 `cmandatory`, `coptional`, `cmandatory_`, `coptional_`

cmandatory :: ( Real u, Floating u, FromPtSize u ) 
           => Box u -> Box u -> TraceDrawing u (Path u)
cmandatory = connWith diskTip

coptional :: ( Real u, Floating u, FromPtSize u ) 
          => Box u -> Box u -> TraceDrawing u (Path u)
coptional = connWith odiskTip


cmandatory_ :: ( Real u, Floating u, FromPtSize u ) 
            => Box u -> Box u -> TraceDrawing u ()
cmandatory_ p0 p1 = connWith diskTip p0 p1 >> return ()

coptional_ :: ( Real u, Floating u, FromPtSize u ) 
           => Box u -> Box u -> TraceDrawing u ()
coptional_ p0 p1 = connWith odiskTip p0 p1 >> return ()