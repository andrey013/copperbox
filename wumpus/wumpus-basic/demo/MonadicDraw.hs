{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module MonadicDraw where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.TurtleMonad
import Wumpus.Basic.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

-- Not currently working - needs a Turtle + Drawing monad...

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/mdraw01.eps" pic1
    writeSVG_latin1 "./out/mdraw01.svg" pic1


pic1 :: DPicture
pic1 = drawGraphicU $ 
           execTurtleDrawing (regularConfig 40) (0,0) (standardAttr 24) $ do
        a <- node dotCircle 
        b <- node $ (dotText "text") `at` (P2 1 2)
        connect' (northeast a) (radialAnchor (5*pi/4) b)
        
connect' :: Num u => Point2 u -> Point2 u -> TurtleDrawing u ()
connect' a b = trace $ wrapG $ ostroke black $ vertexPath [a,b]  


