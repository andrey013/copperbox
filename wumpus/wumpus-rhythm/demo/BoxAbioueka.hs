{-# OPTIONS -Wall #-}

module BoxAbioueka where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.BoxNotation
import Wumpus.Rhythm.Djembe.GraphicInterpretation
import Wumpus.Rhythm.Djembe.HelveticaLoader
import Wumpus.Rhythm.Djembe.MidiInterpretation

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import ZMidi.Emit hiding (localize)             -- package: zmidi-emit

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    either fk sk =<< loadHelveticaMetrics
    writeZMidiRep "./out/simple01.mid" $ barChannelTracks midi_tracks
  where
    fk ss       = putStrLn ss
    sk metrics  = let pic1 = runDrawingU (makeCtx metrics) djembe_drawing
                  in do { writeEPS "./out/box_abioueka01.eps" pic1
                        ; writeSVG "./out/box_abioueka01.svg" pic1 
                        }
    
    midi_tracks = [djembe1, sangban1, kenkeni1]

makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = joinBevel . fontFace helvetica . metricsContext 14

djembe_drawing :: DDrawing
djembe_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ barLocGraphic djembe1                 `at` P2 0 600
   draw $ barLocGraphic sangban1                `at` P2 0 540

   draw $ barLocGraphic kenkeni1                `at` P2 0 420

   draw $ repLocGraphic djembe_accompany1       `at` P2 0 180
   draw $ repLocGraphic djembe_accompany2       `at` P2 0 120
   draw $ repLocGraphic djembe_accompany3       `at` P2 0  60
   draw $ repLocGraphic djembe_accompany4       `at` P2 0   0


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


kenkeni1 :: CBoxKenkeni repr => Bar repr
kenkeni1 = 
    [ [ I bell_and_stroke,  I just_bell,        I rest_note ]
    , [ I bell_and_stroke,  I just_bell,        I rest_note ]
    , [ I bell_and_stroke,  I just_bell,        I rest_note ]
    , [ I bell_and_stroke,  I just_bell,        I rest_note ] 
    ]
  where
    bell_and_stroke = add_bell kenkeni_stroke
    just_bell       = add_bell rest_note



djembe_accompany1 :: CBoxDjembe repr => Bar repr
djembe_accompany1 = 
    [ [ I bass,             I period,           I bass   ]
    , [ I period,           I slap,             I slap   ]
    , [ I period,           I slap,             I period ]
    , [ I tone,             I tone,             I period ]
    ]

djembe_accompany2 :: CBoxDjembe repr => Bar repr
djembe_accompany2 = 
    [ [ I slap,             I period,           I slap   ]
    , [ I period,           I tone,             I tone   ]
    , [ I period,           I slap,             I period ]
    , [ I tone,             I tone,             I period ]
    ]

djembe_accompany3 :: CBoxDjembe repr => Bar repr
djembe_accompany3 = 
    [ [ I bass,             I period,           I period ]
    , [ I bass,             I tone,             I tone   ]
    , [ I bass,             I period,           I period ]
    , [ I bass,             I tone,             I tone   ]
    ]

djembe_accompany4 :: CBoxDjembe repr => Bar repr
djembe_accompany4 = 
    [ [ I slap,             I slap,             I slap   ]
    , [ I period,           I tone,             I tone   ]
    , [ I slap,             I slap,             I slap   ]
    , [ I period,           I tone,             I tone   ]
    ]
