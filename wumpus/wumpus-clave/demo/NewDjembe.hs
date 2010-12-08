{-# OPTIONS -Wall #-}

module NewDjembe1 where

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
   draw $ localize (fillColour black) $ fullstop    `at` P2 200 0
   draw $ localize (fillColour black) $ swingStem   `at` P2 200 0
   draw $ localize (fillColour black) $ flamStem    `at` P2 220 0
   draw $ localize (fillColour black) $ dot         `at` P2 230 0
   draw $ localize (fillColour black) $ flamDot     `at` P2 230 0
   draw $ localize (fillColour black) $ smallLetter 722 'D' `at` P2 250 0
  
   draw $ barLocGraphic abioueka_djembe_call                `at` P2 0 300
   draw $ barLocGraphic abioueka_djembe_accompanyment1      `at` P2 0 150
   


class CBoxNotation repr where
  period           :: repr
  bass             :: repr
  tone             :: repr
  slap             :: repr
  tone_flam        :: repr
  
instance CBoxNotation G where
  period           = G $ makeDjembeNote $ periodNotehead
  bass             = G $ makeDjembeNote $ letterNotehead 667 'B'
  tone             = G $ makeDjembeNote $ dotNotehead
  slap             = G $ makeDjembeNote $ letterNotehead 667 'X'
  tone_flam        = G $ makeDjembeNote $ letterNotehead 556 '?'


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
