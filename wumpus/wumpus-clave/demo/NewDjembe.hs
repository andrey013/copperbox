{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Djembe.Base
import Wumpus.Djembe.DjembeGraphic
import Wumpus.Djembe.GraphicPrimitives


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_attr text_drawing 
    writeEPS "./out/new_djembe01.eps" pic1
    writeSVG "./out/new_djembe01.svg" pic1 


std_attr :: DrawingContext
std_attr = joinBevel $ fontFace helvetica $ standardContext 24

text_drawing :: DDrawing
text_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ textline "PTgd..B" `at` zeroPt
   draw $ localize (fillColour black) $ swingStem   `at` P2 200 0
   draw $ localize (fillColour black) $ flamStem    `at` P2 220 0
  
   draw $ barLocGraphic abioueka_djembe_call                `at` P2 0 300
   draw $ barLocGraphic abioueka_djembe_accompanyment1      `at` P2 0 200
   draw $ barLocGraphic optional_test                       `at` P2 0 100
   


class CBoxNotation repr where
  rest_note        :: repr
  period           :: repr
  bass             :: repr
  tone             :: repr
  slap             :: repr
  muffled_slap     :: repr
  bass_flam        :: repr
  tone_flam        :: repr
  
instance CBoxNotation G where
  rest_note         = G $ makeRestNote
  period            = G $ makeDjembeNote $ periodNotehead
  bass              = G $ makeDjembeNote $ letterNotehead 667 'B'
  tone              = G $ makeDjembeNote $ dotNotehead
  slap              = G $ makeDjembeNote $ letterNotehead 667 'X'
  muffled_slap      = G $ addBaselineStrike $ 
                            makeDjembeNote $ letterNotehead 667 'X'
  bass_flam         = G $ makeFlamNote (letterNotehead 667 'B')
                                       (letterFlamGlyph 667 'B')
  tone_flam         = G $ makeFlamNote dotNotehead dotFlamGlyph


abioueka_djembe_call :: CBoxNotation repr => Bar repr
abioueka_djembe_call = 
    [ [ I tone_flam,    I tone,         I tone   ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I period,       I period ] 
    ]
    

abioueka_djembe_accompanyment1 :: CBoxNotation repr => Bar repr
abioueka_djembe_accompanyment1 = 
    [ [ I bass,         I period,       I bass   ]
    , [ I period,       I slap,         I slap   ]
    , [ I period,       I slap,         I period ]
    , [ I tone,         I tone,         I period ] 
    ]



optional_test :: (CBoxNotation repr, CStroke repr) => Bar repr
optional_test = 
    [ [ I tone,         I rest_note,    I rest_note,    I muffled_slap   ]
    , [ I tone,         I $ dominant $ rest_note
      ,    I $ other_hand $ optional tone, I rest_note ]
    , [ I $ lead_in tone,         I rest_note,    I rest_note,    I rest_note   ]
    , [ I $ accent tone,         I $ accent rest_note,    I $ optional bass, I rest_note ]
    ]
