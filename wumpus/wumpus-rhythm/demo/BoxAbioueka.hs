{-# OPTIONS -Wall #-}

module BoxAbioueka where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.BoxNotation
import Wumpus.Rhythm.Djembe.GraphicInterpretation
import Wumpus.Rhythm.Djembe.HelveticaLoader

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    either fk sk =<< loadHelveticaMetrics
  where
    fk ss       = putStrLn ss
    sk metrics  = let pic1 = runDrawingU (makeCtx metrics) djembe_drawing
                  in do { writeEPS "./out/box_abioueka01.eps" pic1
                        ; writeSVG "./out/box_abioueka01.svg" pic1 
                        }


makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = joinBevel . fontFace helvetica . metricsContext 14

djembe_drawing :: DDrawing
djembe_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ barLocGraphic djembe1                 `at` P2 0 600
   draw $ barLocGraphic sangban1                `at` P2 0 540


djembe1 :: CBoxDjembe repr => Bar repr
djembe1 = 
    [ [ I tone_flam,    I tone,         I tone   ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I period,       I period ] 
    ]
    

sangban1 :: CBoxSangban repr => Bar repr
sangban1 = 
    [ [ I rest_note,        I rest_note,        I rest_note ]
    , [ I rest_note,        I rest_note,        I rest_note ]
    , [ I rest_note,        I rest_note,        I rest_note ]
    , [ I sangban_stroke,   I sangban_stroke,   I rest_note ] 
    ]


