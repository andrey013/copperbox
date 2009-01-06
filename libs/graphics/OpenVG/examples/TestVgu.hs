{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TestVgu
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  LGPL - this is a direct translation of Ivan Leben's code
--                (test_vgu.c) so its LGPL
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Haskell translation of test_vgu from the shivavg examples
--

--------------------------------------------------------------------------------

-- Currently (on Windows) it seems to work best with `runhaskell` ...
-- shell> runhaskell.exe -i../src -lopenvg32 TestVgu.hs



module Main where

import TestUtils

import Graphics.Rendering.OpenVG ( 
        VGPath, VGfloat, 
        ArcType(..), line, polygon, roundRect, ellipse, arc,
        destroyContextSH )
import qualified Graphics.Rendering.OpenVG as VG        
import Graphics.UI.GLUT

import Control.Monad ( zipWithM_ )


display :: [VGPath] -> IO ()
display primitives = do
    VG.clearColor $= white
    sz  <- get $ windowSize
    pos <- get $ windowPosition
    VG.clear pos sz
    VG.matrixMode $= VG.PathUserToSurface
    
    zipWithM_ fn primitives coords
    putStrLn "display - flush..."
    swapBuffers
  where
    white :: Color4 GLfloat
    white = Color4 1.0 1.0 1.0 1.0
    
    coords = [(x,y) | y <- [0..2], x <- [0..2] ]

    fn :: VGPath -> (VGfloat, VGfloat) -> IO ()    
    fn a (x,y) = do 
        VG.loadIdentity
        VG.translate (100 + x*150) (100 + y*150)
        VG.drawPath a [VG.StrokePath]

points :: [VGfloat]
points = [(-30),(-30),  30,(-30),  0,30]
    
createPrimitives :: IO [VGPath]
createPrimitives = do 
    line1 <- testCreatePath 
    line line1 (-30.0) (-30.0) 30.0 30.0
    
    poly_open <- testCreatePath
    polygon poly_open points True
    
    poly_closed <- testCreatePath
    polygon poly_closed points False
    
    rect1 <- testCreatePath
    VG.rect rect1 (-50) (-30) 100 60
    
    rect_round <- testCreatePath
    roundRect rect_round  (-50) (-30) 100 60 30 30
    
    ellipse1 <- testCreatePath
    ellipse ellipse1 0 0 100 60
  
    arc_open <- testCreatePath
    arc arc_open  0 0  100 60  0  270  ArcOpen

    arc_chord <- testCreatePath
    arc arc_chord 0 0 100 60 0 270 ArcChord
  
    arc_pie  <- testCreatePath
    arc arc_pie 0 0 100 60 0 270 ArcPie
  
    return [ line1, poly_open, poly_closed
           , rect1, rect_round, ellipse1
           , arc_open, arc_chord, arc_pie
           ]

     

main :: IO ()
main = do
    win <- testInit (Position 0 0) (Size 500 500) title
    prims <- createPrimitives
    displayCallback $= display prims
    mainLoop
    
    -- end
    destroyContextSH
    destroyWindow win
    
  where
    title =  "Haskell OpenVG: VGU Primitives test"

-- testDisplay :: IO ()

        