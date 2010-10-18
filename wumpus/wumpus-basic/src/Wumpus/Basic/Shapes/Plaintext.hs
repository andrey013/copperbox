{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Plaintext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Plaintext is a bit like a shape but does not generate a path 
-- and cannot be scaled (it can be rotated or translated).
--
-- \*\* WARNING \*\* - the types of Shapes and Plaintext are not
-- ideal and are pending revision.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Plaintext
  (

    PlaintextAnchor
  , DPlaintextAnchor
  , Plaintext
  , DPlaintext

  , plaintext
  , drawText

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative




--------------------------------------------------------------------------------
-- Free label

-- Free label is a rectangle that /is not drawn/, the 
-- constructor should always create some text.

newtype PlaintextAnchor u = PlaintextAnchor  { getPlaintext :: Rectangle u }


type DPlaintextAnchor = PlaintextAnchor Double

type instance DUnit (PlaintextAnchor u) = u


data Plaintext u = Plaintext
      { text_text :: String     -- Note - generalize this for multi-line...
      , text_x    :: !u
      , text_y    :: !u
      , text_ang  :: !Radian
      }
  deriving (Eq,Ord,Show)

type DPlaintext = Plaintext Double

type instance DUnit (Plaintext u) = u


instance (Real u, Floating u) => CenterAnchor (PlaintextAnchor u) where
  center = center . getPlaintext


instance (Real u, Floating u) => CardinalAnchor (PlaintextAnchor u) where
  north = north . getPlaintext
  south = south . getPlaintext
  east  = east . getPlaintext
  west  = west . getPlaintext

instance (Real u, Floating u) => CardinalAnchor2 (PlaintextAnchor u) where
  northeast = northeast . getPlaintext
  southeast = southeast . getPlaintext
  southwest = southwest . getPlaintext
  northwest = northwest . getPlaintext

instance (Real u, Floating u) => RadialAnchor (PlaintextAnchor u) where
  radialAnchor theta = radialAnchor theta . getPlaintext



instance Rotate (Plaintext u) where
  rotate dr = (\s i -> s { text_ang = i+dr }) <*> text_ang

-- Note - cannot scale Plaintext


instance Num u => Translate (Plaintext u) where
  translate dx dy = (\s x y -> s { text_x = x+dx, text_y = y+dy }) 
                      <*> text_x <*> text_y



plaintext :: Num u => String -> Plaintext u
plaintext ss = Plaintext { text_text  = ss
                         , text_x     = 0
                         , text_y     = 0
                         , text_ang   = 0 }




drawText :: (Real u, Floating u, FromPtSize u)
         => Plaintext u -> Image u (PlaintextAnchor u)
drawText x = intoImage (oneLineRect x) (drawOneLine x)



textCTM :: Num u => u -> u -> Radian -> ShapeCTM u
textCTM x y theta = rotate theta $ makeShapeCTM (P2 x y)

oneLineRect :: (Fractional u, Ord u, FromPtSize u) 
        => Plaintext u -> DrawingR (PlaintextAnchor u)
oneLineRect ptext = 
    monoTextDimensions (text_text ptext) >>= \(w,h) -> 
    return (PlaintextAnchor $ mkRectangle (0.5*w) (0.5*h) ctm)
  where
    ctm = textCTM (text_x ptext) (text_y ptext) (text_ang ptext)
                   

drawOneLine :: (Real u, Floating u, FromPtSize u) 
            => Plaintext u -> Graphic u 
drawOneLine (Plaintext { text_text = ss, text_x=dx, text_y=dy
                       , text_ang = ang }) =
    monoVecToCenter ss >>= \v -> 
    let ctr = P2 dx dy; bl = ctr .-^ v in 
    rotTextline ang ss (rotateAbout ang ctr bl)



rotTextline :: (Real u, Floating u) => Radian -> String -> LocGraphic u
rotTextline theta ss baseline_left = 
    withTextAttr $ \rgb attr -> 
        wrapPrim $ rtextlabel rgb attr ss theta baseline_left
