{-# OPTIONS -Wall #-}

module DotPic where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Text.DirectionZero
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
makeCtx = fill_colour peru . set_font helvetica . metricsContext 14


dot_pic :: CtxPicture
dot_pic = drawTracing $ tableGraphic dottable


dottable :: [(String, DotLocImage Double)]
dottable =   
    [ ("smallDisk",     smallDisk)
    , ("largeDisk",     largeDisk)
    , ("smallCirc",     smallCirc)
    , ("largeCirc",     largeCirc)
    , ("dotNone",       dotNone)
    , ("dotHLine",      dotHLine)
    , ("dotVLine",      dotVLine)
    , ("dotX",          dotX)
    , ("dotPlus",       dotPlus)
    , ("dotCross",      dotCross)
    , ("dotDiamond",    dotDiamond)
    , ("dotDisk",       dotDisk)
    , ("dotSquare",     dotSquare)
    , ("dotCircle",     dotCircle)
    , ("dotPentagon",   dotPentagon)
    , ("dotStar",       dotStar)
    , ("dotAsterisk",   dotAsterisk)
    , ("dotOPlus",      dotOPlus)
    , ("dotOCross",     dotOCross)
    , ("dotFOCross",    dotFOCross)
    , ("dotFDiamond",   dotFDiamond)
    , ("dotText" ,      dotText "%")
    , ("dotTriangle",   dotTriangle) 
    ]



tableGraphic :: [(String, DotLocImage Double)] -> TraceDrawing Double ()
tableGraphic imgs = 
    draw $ chain_ chn_alg (map makeDotDrawing imgs) `at` pt
  where
    row_count   = 18
    chn_alg     = tableDown row_count (180,36)
    pt          = displaceV (fromIntegral $ 36 * row_count) zeroPt 




makeDotDrawing :: (String, DotLocImage Double) -> DLocGraphic 
makeDotDrawing (name,df) = 
    drawing `oplus` moveStart (displaceVec $ vec 86 14) lbl
  where
    drawing     = execRelBuild $ 
                    penCtxUpdate path_style >> 
                    insert dot >> mapM (\v -> line v >> insert dot) steps

    lbl         = promoteR1 $ \pt -> fmap ignoreAns $ 
                    atStartAddr (textline name) pt WW

    steps       = [V2 25 15, V2 25 (-15), V2 25 15]
    dot         = locGraphic_ df
    path_style  = packed_dotted . stroke_colour cadet_blue


