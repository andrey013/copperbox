{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader
import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory

import Data.Ratio -- temp

dummy :: Rational -> Int -> Int
dummy r sz = ceiling $ (dsz * fromRational r)
  where
    dsz :: Double
    dsz = fromIntegral sz

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
    drawl (P2 0 0) $ distribH 50 [ fn $ underscore2 $ charNote 'X'
                                 , fn $ parens2 $ charNote 'P'
                                 , fn $ parens2 $ diskNote
                                 , fn $ parens2 $ noNote
                                 , fn $ charNote 'g'
                                 , fn $ periodNote
                                 , fn $ angleStrike $ diskNote
                                 ]

    drawl (P2 0 200) $ runDjembeDraw unit_width_12_8 $
                          drawBeamGroup [ Note NoteDisk
                                        , Note NoteDisk
                                        , Swing $ NoteChar 'X'
                                        , Div NoteDisk NoteDisk
                                        , Note NoteNone
                                        ]

  where
    fn = runPosNoteHead 0
