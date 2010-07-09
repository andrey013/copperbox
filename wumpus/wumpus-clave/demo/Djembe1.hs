{-# OPTIONS -Wall #-}

module Djembe1 where

import Wumpus.Clave.DjembeStrokes

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe
import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/djembe01.eps" pic1
    >> writeSVG_latin1 "./out/djembe01.svg" pic1 


pic1 :: Picture Double
pic1 = picOver textSample $ fromMaybe errK $ drawGraphic line1

errK :: a
errK = error "no picture"

textSample :: DPicture
textSample = frame $ textlabel (black, helvetica 12) "Helvetica 12 pt" (P2 0 (-60))

line1 :: DGraphic
line1 = mk [bass, muffledBass, tone, muffledTone, slap, paren bass, paren slap
           , paren tone, dot, dominant bass, dominant slap, otherhand slap
           , bassFlam, toneFlam, slapFlam, muffledSlap
           ] 


line2 :: DGraphic
line2 = mk [slap , slap, slap, slap, slap, slap
         --  , flam slapP slapP
         --  , flam bassP bassP 
         --  , flam slapP slapP
         --  , flam bassP bassP 
         --  , flam toneP toneP
           ]

mk :: [DGraphicF] -> DGraphic
mk xs = foldr1 (.) $ zipWith fn xs (iterate (.+^ V2 20 0) (P2 0 0))
  where
    fn f pt = (reddot pt . f pt) 
    

reddot :: DGraphicF
reddot = circle red 1


