{-# OPTIONS -Wall #-}

module NewDjembe where

import Wumpus.Rhythm.Djembe.GraphicPrimitives
import Wumpus.Rhythm.Djembe.HelveticaLoader

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Text.PosChar
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory




main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    either fk sk =<< loadHelveticaMetrics
  where
    fk ss       = putStrLn ss
    sk metrics  = let pic1 = runCtxPictureU (makeCtx metrics) djembe_drawing
                  in do { writeEPS "./out/_temp_new_djembe01.eps" pic1
                        ; writeSVG "./out/_temp_new_djembe01.svg" pic1 
                        }

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = join_bevel . set_font helvetica . metricsContext 14

djembe_drawing :: DCtxPicture
djembe_drawing = drawTracing $ localize fill_use_stroke_colour $ do 
   drawli_ zeroPt       $ disk
   drawli_ (P2 20 0)    $ parens $ char 'P'
   drawli_ (P2 40 0)    $ decoHand period domhand
   drawli_ (P2 60 0)    $ decoHand (char 'T') otherhand
   drawli_ (P2 80 0)    $ underscore $ singlestem $ (char 'P')
   drawli_ (P2 100 0)   $ singlestem $ anglestrike (char 'g')
