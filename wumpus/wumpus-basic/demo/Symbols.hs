{-# OPTIONS -Wall #-}

module Symbols where

import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRSymbol
import Wumpus.Basic.Text.LRText


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
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
std_ctx = fontface timesRoman $ standardContext 12

pic1 :: DPicture 
pic1 = liftToPictureU $ execDrawing std_ctx $ do
         zipWithM_ mdraw letters_01 column_01
         zipWithM_ sdraw letters_01 column_01
         zipWithM_ mdraw letters_02 column_02
         zipWithM_ sdraw letters_02 column_02

  where
    mdraw (_,ma) pt = execTextM ma >>= \a -> draw $ a `at` pt
    sdraw (s,_)  pt = draw $ textline s `at` pt .+^ hvec 16

letters_01 :: [(String, TextM Double ())]
letters_01 = 
    [ ("uAlpha",                uAlpha) 
    , ("uBeta",                 uBeta)
    , ("uChi",                  uChi)
    , ("uDelta",                uDelta)
    , ("uEpsilon",              uEpsilon)
    , ("uEta",                  uEta)
    , ("uEuro",                 uEuro)
    , ("uGamma",                uGamma)
    , ("uIfraktur",             uIfraktur)
    , ("uIota",                 uIota)
    , ("uKappa",                uKappa)
    , ("uLambda",               uLambda)
    , ("uMu",                   uMu)
    , ("uNu",                   uNu)
    , ("uOmega",                uOmega)
    , ("uOmicron",              uOmicron)
    , ("uPhi",                  uPhi)
    , ("uPi",                   uPi)
    , ("uPsi",                  uPsi)
    , ("uRfraktur",             uRfraktur)
    , ("uRho",                  uRho)
    , ("uSigma",                uSigma)
    , ("uTau",                  uTau)
    , ("uTheta",                uTheta)
    , ("uUpsilon",              uUpsilon)
    , ("uUpsilon1",             uUpsilon1)
    , ("uXi",                   uXi)
    , ("uZeta",                 uZeta)
    , ("aleph",                 aleph)
    , ("alpha",                 alpha)
    , ("ampersand",             ampersand)
    , ("angle",                 angle)
    , ("angleleft",             angleleft)
    , ("angleright",            angleright)
    , ("approxequal",           approxequal)
    ]

letters_02 :: [(String, TextM Double ())]
letters_02 = 
    [ ("arrowboth",             arrowboth) 
    , ("arrowdblboth",          arrowdblboth)
    , ("arrowdbldown",          arrowdbldown)
    , ("arrowdblleft",          arrowdblleft)
    , ("arrowdblright",         arrowdblright)
    , ("arrowdblup",            arrowdblup)
    , ("arrowdown",             arrowdown)
    , ("arrowleft",             arrowleft)
    , ("arrowright",            arrowright)
    , ("arrowup",               arrowup)
    , ("asteriskmath",          asteriskmath)
    , ("bar",                   bar)
    , ("beta",                  beta)
    , ("braceleft",             braceleft)
    , ("braceright",            braceright)
    , ("bracketleft",           bracketleft)
    , ("bracketright",          bracketright)
    ]

column_01 :: Num u => [Point2 u]
column_01 = iterate (.+^ vvec (-16)) (P2 0 600)

column_02 :: Num u => [Point2 u]
column_02 = iterate (.+^ vvec (-16)) (P2 100 600)
