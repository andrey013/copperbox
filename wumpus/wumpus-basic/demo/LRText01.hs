{-# OPTIONS -Wall #-}

module LRText01 where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.SVGColours
import Wumpus.Basic.Text.LRSymbol
import Wumpus.Basic.Text.LRText


import Wumpus.Core                      -- package: wumpus-core

import Prelude hiding ( pi )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/lrtext01.eps" pic1
    writeSVG_latin1 "./out/lrtext01.svg" pic1

std_attr :: DrawingAttr
std_attr = standardAttr 14

pic1 :: Picture Double 
pic1 = drawGraphicU $ g1 zeroPt . straightLine red (hvec 200) zeroPt

two_line :: Num u => GraphicF u 
two_line = textline (textAttr std_attr) "line one"
                  `cc` (textline (textAttr std_attr) "line two" . vdisp (-16))

g1 :: DGraphicF
g1 = snd $ runTextM 16 (stroke_colour std_attr, font_props std_attr) $ ma
  where
    ma = text "ab" >> text ", cd" >> text ", ef" >> text ", gh" >> char '.'
       >> newline
       >> alpha >> beta >> gamma >> delta >> epsilon >> zeta >> eta
       >> theta >> iota >> kappa >> lambda >> mu >> nu >> xi >> pi
       >> rho >> sigma >> tau >> upsilon >> phi >> chi >> psi >> omega