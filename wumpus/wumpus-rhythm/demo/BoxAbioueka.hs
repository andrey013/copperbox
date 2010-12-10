{-# OPTIONS -Wall #-}

module BoxAbioueka where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.BoxNotation
import Wumpus.Rhythm.Djembe.GraphicInterpretation


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_attr text_drawing 
    writeEPS "./out/box_abioueka01.eps" pic1
    writeSVG "./out/box_abioueka01.svg" pic1 


std_attr :: DrawingContext
std_attr = joinBevel $ fontFace helvetica $ standardContext 14

text_drawing :: DDrawing
text_drawing = drawTracing $ localize bothStrokeColour $ do 
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


