{-# OPTIONS -Wall #-}

module DjembeBoxNotation where

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
    writeEPS "./out/djembe_box_notation01.eps" pic1
    writeSVG "./out/djembe_box_notation01.svg" pic1 


std_attr :: DrawingContext
std_attr = joinBevel $ fontFace helvetica $ standardContext 24

text_drawing :: DDrawing
text_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ barLocGraphic abioueka_djembe_call                `at` P2 0 400
   draw $ barLocGraphic abioueka_djembe_accompanyment1      `at` P2 0 300
   draw $ barLocGraphic optional_test                       `at` P2 0 200
   draw $ barLocGraphic plet_test                           `at` P2 0 100
   draw $ repLocGraphic sixteenths_test                     `at` P2 0   0   


abioueka_djembe_call :: CBoxDjembe repr => Bar repr
abioueka_djembe_call = 
    [ [ I tone_flam,    I tone,         I tone   ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I period,       I period ] 
    ]
    

abioueka_djembe_accompanyment1 :: CBoxDjembe repr => Bar repr
abioueka_djembe_accompanyment1 = 
    [ [ I bass,         I period,       I bass   ]
    , [ I period,       I slap,         I slap   ]
    , [ I period,       I slap,         I period ]
    , [ I tone,         I tone,         I period ] 
    ]



optional_test :: CBoxDjembe repr => Bar repr
optional_test = 
    [ [ I tone,         I muffled_tone,    I muffled_bass,    I muffled_slap   ]
    , [ I tone,         I $ dominant $ rest_note
      ,    I $ other_hand $ optional tone, I rest_note ]
    , [ I $ lead_in tone,         I rest_note,    I rest_note,    I rest_note   ]
    , [ I $ accent tone,         I $ accent rest_note,    I $ optional bass, I rest_note ]
    ]



plet_test :: CBoxDjembe repr => Bar repr
plet_test = 
    [ [ Pl 3 2 [slap, slap, slap], I slap, I period ] 
    , [ I slap, I period, I period, I period ] 
    ]


sixteenths_test :: CBoxDjembe repr => Bar repr
sixteenths_test = 
    [ [ Ha slap slap,   I slap,         I slap ] 
    , [ I slap,         I slap,         I rest_note ] 
    , [ Ha tone tone,   I tone,         I tone ] 
    , [ I tone,         I period,       I period ] 
    ]