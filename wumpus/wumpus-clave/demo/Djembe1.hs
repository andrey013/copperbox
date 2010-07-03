{-# OPTIONS -Wall #-}

module Djembe1 where

import Wumpus.Clave.ClaveMonad
import Wumpus.Clave.DjembeStrokes
import Wumpus.Clave.Drawing

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SVGColours

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe
import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/djembe01.eps" pic1
    >> writeSVG_latin1 "./out/djembe01.svg" pic1 


pic1 :: Picture Double
pic1 = uniformScale 2 $ fromMaybe errK $ drawGraphic line1

errK :: a
errK = error "no picture"

line1 :: DGraphic
line1 = mk [bass, muffledBass, tone, muffledTone, slap, paren bass, paren slap
           , paren tone, dot, dominant bass, dominant slap, otherhand slap ] 

mk :: [DGraphicF] -> DGraphic
mk xs = foldr1 (.) $ zipWith fn xs (iterate (.+^ V2 14 0) (P2 0 0))
  where
    fn f pt = (reddot pt . f pt) 
    

reddot :: DGraphicF
reddot = circle red 1