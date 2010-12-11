{-# OPTIONS -Wall #-}

module DjembeBoxNotation where

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
                  in do { writeEPS "./out/djembe_box_notation01.eps" pic1
                        ; writeSVG "./out/djembe_box_notation01.svg" pic1 
                        }




makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = joinBevel . fontFace helvetica . metricsContext 14


djembe_drawing :: DDrawing
djembe_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ barLocGraphic sixteen_eighths                     `at` P2 0 600
   draw $ barLocGraphic triplets_16_8                       `at` P2 0 540
   draw $ barLocGraphic sixteenths_16_8                     `at` P2 0 480
   draw $ barLocGraphic swing_16_8                          `at` P2 0 420
   draw $ barLocGraphic twelve_eighths                      `at` P2 0 360
   draw $ barLocGraphic sixteenths_12_8                     `at` P2 0 300
   draw $ barLocGraphic swing_12_8                          `at` P2 0 240


sixteen_eighths :: CBoxDjembe repr => Bar repr
sixteen_eighths = 
    [ [ I rest_note,    I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note,    I rest_note ]
    ]

triplets_16_8 :: CBoxDjembe repr => Bar repr
triplets_16_8 = 
    [ [ Pl 3 2 [slap, slap, slap],      I slap,         I period ] 
    , [ I tone,         I tone,         I period,       I period ]
    , [ Pl 3 2 [slap, slap, slap],      I slap,         I period ] 
    , [ I tone,         I tone,         I period,       I period ]
    ]     


sixteenths_16_8 :: CBoxDjembe repr => Bar repr
sixteenths_16_8 = 
    [ [ I tone,         I tone,         I slap,         I tone   ]
    , [ I tone,         I slap,         I tone,         I tone   ]
    , [ I slap,         I period,       Ha slap slap,   I slap   ]
    , [ I slap,         I slap,         I slap,         I period ]
    ]

swing_16_8 :: CBoxDjembe repr => Bar repr
swing_16_8 = 
    [ [ I slap,         I period,       I period,       I slap   ]
    , [ I slap,         I period,       I tone,         S tone   ]
    , [ I slap,         I period,       I period,       I slap   ]
    , [ I slap,         I period,       I tone,         S tone   ]
    ]

-- Note - wumpus-rhythm is not proportional...
--

twelve_eighths :: CBoxDjembe repr => Bar repr
twelve_eighths = 
    [ [ I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note ]
    , [ I rest_note,    I rest_note,    I rest_note ]
    ]


sixteenths_12_8 :: CBoxDjembe repr => Bar repr
sixteenths_12_8 = 
    [ [ Ha slap slap,   I slap,         I slap          ]
    , [ I slap,         I slap,         I rest_note     ]
    , [ Ha tone tone,   I tone,         I tone          ]
    , [ I slap,         I period,       I period        ]
    ]


swing_12_8 :: CBoxDjembe repr => Bar repr
swing_12_8 = 
    [ [ I tone,         S slap,         I slap  ]
    , [ S tone,         I tone,         S slap  ]
    , [ I slap,         S tone,         I tone  ]
    , [ S slap,         I slap,         S tone  ]
    ]

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