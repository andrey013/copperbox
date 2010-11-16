{-# OPTIONS -Wall #-}

-- WARNING - This example is now considered obsolete.
-- LRText no longer seems a satisfactory way to build text.

module LRText01 where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.DrawingComposition
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRSymbol
import Wumpus.Basic.Text.LRText


import Wumpus.Core                      -- package: wumpus-core

import Prelude hiding ( pi )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic = runDrawingU std_ctx combo_pic
    writeEPS "./out/lrtext01.eps" pic
    writeSVG "./out/lrtext01.svg" pic

std_ctx :: DrawingContext
std_ctx = fontface times_roman $ standardContext 48

combo_pic :: DDrawing
combo_pic = vcat [pic1,pic2,pic3]


pic1 :: DDrawing
pic1 = drawTracing $ draw $ hkernline ks `at` zeroPt
  where
    ks :: [KerningChar Double]
    ks = [ kernchar 0 'A', kernchar 34 'B', kernchar 29 'C' ] 

-- 
pic2 :: DDrawing
pic2 = drawTracing $ draw $ hkernline ks `at` zeroPt
  where
    ks :: [KerningChar Double]
    ks = map (kernchar 0) $ "No kerning" 


pic3 :: DDrawing
pic3 = drawTracing $ do
         let abc = execLRText (char 'a' >> char 'b' >> epsilon >> char 'c')
         draw $ abc `at` (P2 0 3) 
         localize (strokeColour red) $ draw $ straightLine (hvec 200) `at` zeroPt



g1 :: LRText Double ()
g1 =   alpha >> beta >> gamma >> delta >> epsilon >> zeta >> eta
    >> theta >> iota >> kappa >> lambda >> mu >> nu >> xi >> pi
    >> rho >> sigma >> tau >> upsilon >> phi >> chi >> psi >> omega
    >> uGamma >> uDelta >> uTheta >> uLambda >> uXi >> uPi >> kern 2 >> uSigma
    >> uUpsilon >> uPhi >> kern 2 >>  uPsi >> kern 2 >> uOmega

