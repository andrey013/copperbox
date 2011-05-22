{-# OPTIONS -Wall #-}

module Abioueka where

import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GDgdPT
import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader
import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    either fk sk =<< loadHelveticaMetrics helvetica
  where
    fk ss       = putStrLn ss
    sk metrics  = let out = runCtxPictureU (makeCtx metrics) pic01
                  in do { writeEPS "./out/abioueka.eps" out
                        ; writeSVG "./out/abioueka.svg" out 
                        }

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fill_colour black . set_font helvetica . metricsContext 13


pic01 :: CtxPicture
pic01 = udrawTracing (0::Double) $ do

    drawl (P2 0 270) $ runDjembeDraw unit_width_12_8 sangban1
    drawl (P2 0 220) $ runDjembeDraw unit_width_12_8 sdoundounba1
    drawl (P2 0 150) $ runDjembeDraw unit_width_12_8 accomp1
    drawl (P2 0 100) $ runDjembeDraw unit_width_12_8 accomp2
    drawl (P2 0  50) $ runDjembeDraw unit_width_12_8 accomp3
    drawl (P2 0   0) $ runDjembeDraw unit_width_12_8 accomp4



-- | sangban

sangban1 :: DjembeDraw ()
sangban1 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup c >> beamGroup d
  where
    a = notes [ sangban_hi_stroke, blank, sangban_hi_stroke ]
    b = notes [ blank, sangban_stroke_plus_hi, sangban_stroke_plus_hi ]
    c = notes [ blank, sangban_pressed_stroke_plus_hi, blank ]
    d = [ leadin $ note sangban_stroke_plus_hi, note sangban_stroke_plus_hi, note blank ]


-- | single doundounba

sdoundounba1 :: DjembeDraw ()
sdoundounba1 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup c >> beamGroup b
  where
    a = notes [ doundounba_stroke_plus_hi, period, doundounba_stroke_plus_hi ]
    b = notes [ blank, doundounba_hi_stroke, blank ]
    c = notes [ doundounba_hi_stroke, blank, doundounba_hi_stroke ]


-- | Accompanying djembes

accomp1 :: DjembeDraw ()
accomp1 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup c >> beamGroup d
  where
    a = notes [ bass_Gun, period,  bass_Gun ]
    b = notes [ period,   slap_Pa, slap_Ta ]
    c = notes [ period,   slap_Ta, period  ]
    d = notes [ tone_do,  tone_go, period  ]

accomp2 :: DjembeDraw ()
accomp2 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup c >> beamGroup d
  where
    a = notes [ slap_Pa, period, slap_Pa ]
    b = notes [ period,   tone_go, tone_do ]
    c = notes [ period,   slap_Ta, period  ]
    d = notes [ tone_do,  tone_go, period  ]

accomp3 :: DjembeDraw ()
accomp3 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup a >> beamGroup b
  where
    a = notes [ bass_Gun, period, period ]
    b = notes [ bass_Dun, tone_go, tone_do ]


accomp4 :: DjembeDraw ()
accomp4 = inrepeat $ 
    beamGroup a >> beamGroup b >> beamGroup a >> beamGroup b
  where
    a = notes [ slap_Pa, slap_Ta, slap_Pa ]
    b = notes [ period, tone_go, tone_do ]