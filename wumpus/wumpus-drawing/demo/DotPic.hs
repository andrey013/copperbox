{-# OPTIONS -Wall #-}

module DotPic where



import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Text.SafeFonts

import FontLoaderUtils

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

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
    base_metrics <- loadGSFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) dot_pic
    writeEPS "./out/dot_pic01_gs.eps" pic1
    writeSVG "./out/dot_pic01_gs.svg" pic1

 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) dot_pic
    writeEPS "./out/dot_pic01_afm.eps" pic1
    writeSVG "./out/dot_pic01_afm.svg" pic1

 
makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fill_colour peru . set_font helvetica . metricsContext 24


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
tableGraphic imgs = 
    drawi_ $ chn (map makeDotDrawing imgs) `at` pt
  where
    row_count   = length imgs
    chn         = tableDown row_count (1,36)
    pt          = displaceV (fromIntegral $ 36 * row_count) zeroPt 



-- This is a bit convoluted - maybe there should be chain-run 
-- functions for TraceDrawings as well as LocGraphics?

makeDotDrawing :: (Real u, Floating u, FromPtSize u) 
               => DotLocImage u -> LocGraphic u
makeDotDrawing dotF = 
    promoteR1 $ \pt -> 
        let all_points = map (pt .+^) displacements
        in oconcat (dashline all_points)
                   (map (\p1 -> ignoreL $ dotF `at` p1) all_points)
  where
    dashline = \ps -> localize attrUpd (openStroke $ vertexPath ps)

    attrUpd  :: DrawingContext -> DrawingContext
    attrUpd  = packed_dotted . stroke_colour cadet_blue

    ignoreL  = fmap (replaceL uNil) 

displacements :: Num u => [Vec2 u]
displacements = [V2 0 0, V2 64 20, V2 128 0, V2 192 20]


-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs

