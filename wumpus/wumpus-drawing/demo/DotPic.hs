{-# OPTIONS -Wall #-}

module DotPic where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid
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


-- Note - dots should probably have lower_case_with_underscore 
-- names.

dottable :: [(String, DotLocImage Double)]
dottable =   
    [ ("smallDisk",     smallDisk)
    , ("largeDisk",     largeDisk)
    , ("smallCirc",     smallCirc)
    , ("largeCirc",     largeCirc)
    , ("dotNone",       dotNone)
    , ("dotHBar",       dotHBar)
    , ("dotVBar",       dotVBar)
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
    drawl pt $ distribColumnwiseTable row_count (180,36) 
             $ map makeDotDrawing imgs
  where
    row_count   = 18
    pt          = displace (vvec $ fromIntegral $ 36 * row_count) zeroPt 




makeDotDrawing :: (String, DotLocImage Double) -> DLocGraphic 
makeDotDrawing (name,df) = 
    drawing `mappend` moveStart (vec 86 14) lbl
  where
    drawing     = runPathSpec_ OSTROKE path_spec

    path_spec   = updatePen path_style >>
                  insertl dot >> 
                  mapM (\v -> penline v >> insertl dot) steps >>
                  ureturn
                                
                           

    lbl         = ignoreAns $ promoteLoc $ \pt -> 
                    textline WW name `at` pt

    steps       = [V2 25 15, V2 25 (-15), V2 25 15]
    dot         = ignoreAns df
    path_style  = packed_dotted . stroke_colour cadet_blue


