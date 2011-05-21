{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.Draw
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
    drawl (P2 0 0) $ smallCirc
    drawl (P2 0 0) $ distribH 50 [ fn $ underscore $ charNote 'X'
                                 , fn $ parens $ charNote 'P'
                                 , fn $ parens $ diskNote
                                 , fn $ parens $ noNote
                                 , fn $ charNote 'g'
                                 , fn $ periodNote
                                 , fn $ angleStrike $ diskNote
                                 ]

    drawl (P2 0 100) $ smallCirc
    drawl (P2 0 100) $ runDjembeDraw unit_width_12_8 $ do
        { lrepeat
--        ; accent (leadinAccent)
        ; accent domHand
        ; drawBeamGroup [ Note $ strike disk
                        , Note $ muffled disk
                        , Swing $ optional (NoteChar 'X', zeroDeco)
                        , Div disk disk
                        , Note (NoteNone, zeroDeco)
                        , Flam disk FlamDisk
                        ]
        ; rrepeat
        }
     
    drawl (P2 0 200) $ runDjembeDraw unit_width_12_8 $ drawBeamGroups simple1
    drawl (P2 0 200) $ uconvF $ pletBracket 4000 6
    drawl (P2 0 200) $ smallCirc
  where
    fn = runPosNoteHead 0
    disk = (NoteDisk, zeroDeco)

simple1 :: [[Note]]
simple1 = [a,b,b,c]
  where
    a = [ Flam disk FlamDisk, Note disk, Note disk ]
    b = [ Note disk, Note disk, Note period ]
    c = [ Note disk, Note period, Note period ]
      
    disk = (NoteDisk, zeroDeco)
    period = (NotePeriod, zeroDeco)