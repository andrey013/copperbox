{-# OPTIONS -Wall #-}


module NewText where


import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.FontLoader.AfmV2
import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.Advance

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "C:/cygwin/usr/share/ghostscript/fonts"


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    base_metrics <- loadBaseGlyphMetrics loader ["Times-Roman"]
    let pic1 = runDrawingU (makeStdCtx base_metrics) text_drawing 
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1
  where
    loader = ghostScriptFontLoader font_directory

makeStdCtx :: BaseGlyphMetrics -> DrawingContext
makeStdCtx = fontface times_roman . metricsContext 18


type CatF u = AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u

dummyText :: (Fractional u, Ord u, FromPtSize u) 
          => CatF u -> CF (AdvanceMulti u)
dummyText op = (((mk1 "One Two Three" `join` mk1 "Four")
                  `join` mk1 "Five") `join` mk1 "Six") `join` mk1 "Seven"
  where
    mk1 ss = postpro oneLineH $ singleLine ss 
    join a b = postcomb op a b 


text_drawing :: DDrawing
text_drawing = drawTracing $ do
          drawi_ $ right_text  `at` P2 0 200
          drawi_ $ left_text   `at` P2 0 100
          drawi_ $ center_text `at` P2 0   0



right_text :: (Fractional u, Ord u, FromPtSize u) 
           => LocImage u (BoundingBox u)
right_text = localize (strokeColour red) $ 
               runAdvanceMulti =<< (dummyText $ alignRightH 16)

left_text :: (Fractional u, Ord u, FromPtSize u) 
          => LocImage u (BoundingBox u)
left_text = localize (strokeColour green) $ 
               runAdvanceMulti =<< (dummyText $ alignLeftH 16)

center_text :: (Fractional u, Ord u, FromPtSize u) 
            => LocImage u (BoundingBox u)
center_text = localize (strokeColour blue) $ 
               runAdvanceMulti =<< (dummyText $ alignCenterH 16)


