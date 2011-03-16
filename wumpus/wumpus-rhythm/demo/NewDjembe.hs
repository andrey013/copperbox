{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.PosChar
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory

import Wumpus.Basic.System.FontLoader.Base.Datatypes



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    either fk sk =<< loadHelveticaMetrics
  where
    fk ss       = putStrLn ss
    sk metrics  = let pic1 = runCtxPictureU (makeCtx metrics) djembe_drawing
                  in do { writeEPS "./out/new_djembe01.eps" pic1
                        ; writeSVG "./out/new_djembe01.svg" pic1 
                        }

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = join_bevel . set_font helvetica . metricsContext 24

djembe_drawing :: CtxPicture
djembe_drawing = udrawTracing (0::Double) $ 
    localize (snap_grid_factors 20 20 . fill_use_stroke_colour) $ do 
      draw $ local_ctx (stroke_colour red) $ locStraightLine (hvec 200) `at` zeroPt
      drawli_ zeroPt       $ beamgroup [tone, tone, bassB, period]

