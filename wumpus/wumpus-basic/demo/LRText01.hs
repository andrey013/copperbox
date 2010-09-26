{-# OPTIONS -Wall #-}

module LRText01 where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.PictureLanguage
import Wumpus.Basic.SafeFonts
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
    writeEPS_latin1 "./out/lrtext01.eps" combo_pic
    writeSVG_latin1 "./out/lrtext01.svg" combo_pic

std_ctx :: DrawingContext
std_ctx = fontface timesRoman $ standardContext 48

combo_pic :: DPicture
combo_pic = vcat pic1 [pic2,pic3]


pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing std_ctx $ 
          draw $ hkernline ks `at` zeroPt
  where
    ks :: [KerningChar Double]
    ks = [ kernchar 0 'A', kernchar 34 'B', kernchar 29 'C' ] 

-- 
pic2 :: DPicture
pic2 = liftToPictureU $ execDrawing std_ctx $ 
          draw $ hkernline ks `at` zeroPt
  where
    ks :: [KerningChar Double]
    ks = map (kernchar 0) $ "No kerning" 


pic3 :: DPicture 
pic3 = liftToPictureU $ execDrawing std_ctx $ do
         let abc = execLRText (char 'a' >> char 'b' >> epsilon >> char 'c')
         draw $ abc `at` (P2 0 3) 
         localCtx (primaryColour red) $ draw $ straightLine (hvec 200) `at` zeroPt



g1 :: LRText Double ()
g1 =   alpha >> beta >> gamma >> delta >> epsilon >> zeta >> eta
    >> theta >> iota >> kappa >> lambda >> mu >> nu >> xi >> pi
    >> rho >> sigma >> tau >> upsilon >> phi >> chi >> psi >> omega
    >> uGamma >> uDelta >> uTheta >> uLambda >> uXi >> uPi >> kern 2 >> uSigma
    >> uUpsilon >> uPhi >> kern 2 >>  uPsi >> kern 2 >> uOmega

