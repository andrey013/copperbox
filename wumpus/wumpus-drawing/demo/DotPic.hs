{-# OPTIONS -Wall #-}

module DotPic where



import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import System.Directory

main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) dot_pic
    writeEPS "./out/dot_pic.eps" pic1
    writeSVG "./out/dot_pic.svg" pic1

 
 
makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fill_colour peru . set_font helvetica . metricsContext 24


dot_pic :: CtxPicture
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


tableGraphic :: [DotLocImage Double] -> TraceDrawing Double ()
tableGraphic imgs = 
    draw $ chn (map makeDotDrawing imgs) `at` pt
  where
    row_count   = length imgs
    chn         = tableDown row_count (1,36)
    pt          = displaceV (fromIntegral $ 36 * row_count) zeroPt 



-- This is a bit convoluted - maybe there should be chain-run 
-- functions for TraceDrawings as well as LocGraphics?

makeDotDrawing :: (Real u, Floating u, InterpretUnit u) 
               => DotLocImage u -> LocGraphic u
makeDotDrawing dotF = 
    promoteR1 $ \pt -> 
        let all_points = map (pt .+^) displacements
        in oconcat (dashline all_points)
                   (map (\p1 -> fmap ignoreAns $ dotF `at` p1) all_points)
  where
    dashline = \ps -> localize attrUpd $ vertexPP ps >>= openStroke

    attrUpd  :: DrawingContext -> DrawingContext
    attrUpd  = packed_dotted . stroke_colour cadet_blue

displacements :: Num u => [Vec2 u]
displacements = [V2 0 0, V2 64 20, V2 128 0, V2 192 20]


-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs

