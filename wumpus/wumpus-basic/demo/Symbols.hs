{-# OPTIONS -Wall #-}

module Symbols where

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
    demo01


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/symbols.eps" pic1
    writeSVG_latin1 "./out/symbols.svg" pic1

std_ctx :: DrawingContext
std_ctx = fontface timesRoman $ standardContext 24

pic1 :: DPicture 
pic1 = liftToPictureU $ execDrawing std_ctx $ do
         mdraw greek_lower (P2 0 100)
         mdraw greek_upper (P2 0 75)
  where
    mdraw ma pt = ma >>= \a -> draw $ a `at` pt


greek_lower :: (Num u, FromPtSize u) => Drawing u (LocGraphic u)
greek_lower = execTextM $ 
       alpha >> beta >> gamma >> delta >> epsilon >> zeta >> eta
    >> theta >> iota >> kappa >> lambda >> mu >> nu >> xi >> pi
    >> rho >> sigma >> tau >> upsilon >> phi >> chi >> psi >> omega


greek_upper :: (Num u, FromPtSize u) => Drawing u (LocGraphic u)
greek_upper = execTextM $ 
       uGamma >> uDelta >> uTheta >> uLambda >> uXi >> uPi >> kern 2 >> uSigma
    >> uUpsilon >> uPhi >> kern 2 >>  uPsi >> kern 2 >> uOmega
