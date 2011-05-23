{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GDgdPT
import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader
import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing
import Wumpus.Drawing.Dots.SimpleDots
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
                  in do { writeEPS "./out/new_djembe01.eps" out
                        ; writeSVG "./out/new_djembe01.svg" out 
                        }

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fill_colour black . set_font helvetica . metricsContext 16


pic01 :: CtxPicture
pic01 = udrawTracing (0::Double) $ do
    drawl (P2 0 100) $ runDjembeDraw unit_width_12_8 $ do
        { lrepeat
--        ; accent (leadinAccent)
        ; accent domHand
        ; beamGroup [ N atoke_bass_drum_bell noAnno
                    , N doundounba_pressed_stroke noAnno
                    , S kenkeni_pressed_stroke noAnno
                    , N kenkeni_lo_stroke noAnno
                    , D disk blank noAnno
                    , N shekere_downstroke_plus_hand_hit noAnno
                    ]
        ; rrepeat
        }
     
    drawl (P2 0 200) $ runDjembeDraw unit_width_12_8 $ drawBeamGroups simple1
    drawl (P2 0 180) $ uconvF $ pletBracket 4000 12
    drawl (P2 0 300) $ uconvF $ get $ shekere_downstroke_plus_hand_hit

  where
    get n = localize (set_font_size 24) $ illustratePosObject $ notehead_base n 
    disk = kenkeni_stroke


simple1 :: [[Note]]
simple1 = [a,b,b,c]
  where
    a = [ bass_flam_DGun , note disk, note disk ]
    b = notes [ disk, disk, period ]
    c = notes [ disk, period, period ]
      
    disk = kenkeni_stroke
