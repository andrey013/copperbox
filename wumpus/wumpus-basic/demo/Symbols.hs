{-# OPTIONS -Wall #-}

module Symbols where

import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRSymbol
import Wumpus.Basic.Text.LRText


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import Prelude hiding ( pi, product )

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
         zipWithM_ mdraw letters_03 column_03
         zipWithM_ sdraw letters_03 column_03
         zipWithM_ mdraw letters_04 column_04
         zipWithM_ sdraw letters_04 column_04
         zipWithM_ mdraw letters_05 column_05
         zipWithM_ sdraw letters_05 column_05
         zipWithM_ mdraw letters_06 column_06
         zipWithM_ sdraw letters_06 column_06

  where
    mdraw (_,ma) pt = draw $ execLRText ma `at` pt
    sdraw (s,_)  pt = draw $ textline s `at` pt .+^ hvec 16

letters_01 :: [(String, LRText Double ())]
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

letters_02 :: [(String, LRText Double ())]
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
    , ("bullet",                bullet)
    , ("carriagereturn",        carriagereturn)
    , ("chi",                   chi)
    ]

letters_03 :: [(String, LRText Double ())]
letters_03 = 
    [ ("circlemultiply",        circlemultiply) 
    , ("circleplus",            circleplus)
    , ("club",                  club)
    , ("colon",                 colon)
    , ("comma",                 comma)
    , ("congruent",             congruent)
    , ("copyrightsans",         copyrightsans)
    , ("copyrightserif",        copyrightserif)
    , ("degree",                degree)
    , ("delta",                 delta)
    , ("diamond",               diamond)
    , ("divide",                divide)
    , ("dotmath",               dotmath)
    , ("eight",                 eight)
    , ("element",               element)
    , ("ellipsis",              ellipsis)
    , ("emptyset",              emptyset)
    , ("epsilon",               epsilon)
    , ("equal",                 equal)
    , ("equivalence",           equivalence)
    , ("eta",                   eta)
    , ("exclam",                exclam)
    , ("existential",           existential)
    , ("five",                  five)
    , ("florin",                florin)
    , ("four",                  four)
    , ("fraction",              fraction)
    , ("gamma",                 gamma)
    , ("gradient",              gradient)
    , ("greater",               greater)
    , ("greaterequal",          greaterequal)
    , ("heart",                 heart)
    , ("infinity",              infinity)
    , ("integral",              integral)
    ]


letters_04 :: [(String, LRText Double ())]
letters_04 = 
    [ ("intersection",          intersection)
    , ("iota",                  iota)
    , ("kappa",                 kappa)
    , ("lambda",                lambda)
    , ("less",                  less)
    , ("lessequal",             lessequal)
    , ("logicaland",            logicaland)
    , ("logicalnot",            logicalnot)
    , ("logicalor",             logicalor)
    , ("lozenge",               lozenge)
    , ("minus",                 minus)
    , ("minute",                minute)
    , ("mu",                    mu)
    , ("multiply",              multiply)
    , ("nine",                  nine)
    , ("notelement",            notelement)
    , ("notequal",              notequal)
    , ("notsubset",             notsubset)
    , ("nu",                    nu)
    , ("numbersign",            numbersign)
    , ("omega",                 omega)
    , ("omega1",                omega1)
    , ("omicron",               omicron)
    , ("one",                   one)
    , ("parenleft",             parenleft)
    , ("parenright",            parenright)
    ]

letters_05 :: [(String, LRText Double ())]
letters_05 = 
    [ ("partialdiff",           partialdiff)
    , ("percent",               percent)
    , ("period",                period)
    , ("perpendicular",         perpendicular)
    , ("phi",                   phi)
    , ("phi1",                  phi1)
    , ("pi",                    pi)
    , ("plus",                  plus)
    , ("plusminus",             plusminus)
    , ("product",               product)
    , ("propersubset",          propersubset)
    , ("propersuperset",        propersuperset)
    , ("proportional",          proportional)
    , ("psi",                   psi)
    , ("question",              question)
    , ("radical",               radical)
    , ("radicalex",             radicalex)
    , ("reflexsubset",          reflexsubset)
    , ("reflexsuperset",        reflexsuperset)
    , ("registersans",          registersans)
    , ("registerserif",         registerserif)
    , ("rho",                   rho)
    ]

letters_06 :: [(String, LRText Double ())]
letters_06 = 
    [ ("second",                second)
    , ("semicolon",             semicolon)
    , ("seven",                 seven)
    , ("sigma",                 sigma)
    , ("sigma1",                sigma1)
    , ("similar",               similar)
    , ("six",                   six)
    , ("slash",                 slash)
    , ("space",                 space)
    , ("spade",                 spade)
    , ("suchthat",              suchthat)
    , ("summation",             summation)
    , ("tau",                   tau)
    , ("therefore",             therefore)
    , ("theta",                 theta)
    , ("theta1",                theta1)
    , ("three",                 three)
    , ("trademarksans",         trademarksans)
    , ("trademarkserif",        trademarkserif)
    , ("two",                   two)
    , ("underscore",            underscore)
    , ("union",                 union)
    , ("universal",             universal)
    , ("upsilon",               upsilon)
    , ("weierstrass",           weierstrass)
    , ("xi",                    xi)
    , ("zero",                  zero)
    , ("zeta",                  zeta)
    ]

-- TODO - this should use a chain when the Chain API stablizes.

column_01 :: Num u => [Point2 u]
column_01 = iterate (.+^ vvec (-16)) (P2 0 600)

column_02 :: Num u => [Point2 u]
column_02 = iterate (.+^ vvec (-16)) (P2 100 600)

column_03 :: Num u => [Point2 u]
column_03 = iterate (.+^ vvec (-16)) (P2 200 600)

column_04 :: Num u => [Point2 u]
column_04 = iterate (.+^ vvec (-16)) (P2 300 600)

column_05 :: Num u => [Point2 u]
column_05 = iterate (.+^ vvec (-16)) (P2 400 600)

column_06 :: Num u => [Point2 u]
column_06 = iterate (.+^ vvec (-16)) (P2 500 600)
