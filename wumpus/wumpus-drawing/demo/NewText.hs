{-# OPTIONS -Wall #-}


module NewText where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [Right helvetica_family]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/new_text.eps" pic1
    writeSVG "./out/new_text.svg" pic1

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = localize text_margin_tight  $ do
    draw $ (fn SS VALIGN_LEFT)   `at` P2 0 300
    draw $ redPlus `at` zeroPt

    draw $ (fn SS VALIGN_CENTER) `at` P2 0 150
    draw $ redPlus `at` P2 0 150

    draw $ (fn SS VALIGN_RIGHT)  `at` P2 0 0
    draw $ redPlus `at` P2 0 300
  where
    fn addr va = illustrateBoundedLocGraphic $ 
                   runPosObjectBBox addr $ runDoc va helvetica_family doc1


redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) dotPlus

doc1 :: DocGraphic Double
doc1 = vcat $ 
    [ string "Further work"
    , underline $ (localize (set_font_size 36) $ string "on")
           <+> (localize (text_colour red) $ string "multiline")
           <+> string "text"
    , string "and" <> highlight light_blue (string "other things.")
    , strikethrough $ float (sin (0.5::Double))
    , bold (string "Now with bold") <+> italic (string "and italic.") 
    ] 