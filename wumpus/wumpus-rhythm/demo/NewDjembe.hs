{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader

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
                  in do { writeEPS "./out/new_djembe01.eps" out
                        ; writeSVG "./out/new_djembe01.svg" out 
                        }

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = join_bevel . fill_colour black . set_font helvetica . metricsContext 36


pic01 :: CtxPicture
pic01 = udrawTracing (0::Double) $ do
    drawl (P2 0 0) $ distribH 50 [ runNoteHead $ underscore2 $ charNote 'X'
                                 , runNoteHead $ parens2 $ charNote 'P'
                                 , runNoteHead $ parens2 $ diskNote
                                 , runNoteHead $ parens2 $ noNote
                                 , runNoteHead $ charNote 'g'
                                 , runNoteHead $ periodNote
                                 , runNoteHead $ angleStrike $ diskNote
                                 ]

    drawl (P2 0 200) $ drawBeamGroup 60 $ [ Flam $ parens2 $ diskNote
                                          , Note $ parens2 $ diskNote
                                          , Swing $ charNote 'X'
                                          ]
    


pic02 :: CtxPicture
pic02 = udrawTracing (0::Double) $ 
    localize (snap_grid_factors 20 20 . fill_use_stroke_colour) $ do 
      draw $ localize (stroke_colour red) $ locStraightLine (hvec 200) `at` zeroPt
      drawl zeroPt   $ beamgroup [tone, tone, bassB, period]

   
