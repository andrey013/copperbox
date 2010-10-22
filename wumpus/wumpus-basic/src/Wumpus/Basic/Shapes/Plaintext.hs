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
-- \*\* WARNING \*\* - the type of Plaintext is not ideal. This
-- module is pending substantial revision.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Plaintext
  (

    PlaintextAnchor
  , DPlaintextAnchor
  , Plaintext
  , DPlaintext
  , LocPlaintext

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
      { text_ctm  :: ShapeCTM u
      , text_text :: String     -- Note - generalize this for multi-line...
      }
  deriving (Eq,Ord,Show)

type DPlaintext = Plaintext Double

type instance DUnit (Plaintext u) = u

type LocPlaintext u = Point2 u -> Plaintext u

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


updateCTM :: (ShapeCTM u -> ShapeCTM u) -> Plaintext u -> Plaintext u
updateCTM fn = (\s i -> s { text_ctm = fn i }) <*> text_ctm


instance (Real u, Floating u) => Rotate (Plaintext u) where
  rotate r = updateCTM (rotate r)

instance (Real u, Floating u) => RotateAbout (Plaintext u) where
  rotateAbout r pt = updateCTM (rotateAbout r pt)

-- Note scaling doe not scale the text...

instance Num u => Scale (Plaintext u) where
  scale sx sy = updateCTM (scale sx sy)


instance Num u => Translate (Plaintext u) where
  translate dx dy = updateCTM (translate dx dy)



plaintext :: Num u => String -> LocPlaintext u
plaintext ss pt = Plaintext { text_ctm  = makeShapeCTM pt
                            , text_text = ss }



drawText :: (Real u, Floating u, FromPtSize u)
         => Plaintext u -> Image u (PlaintextAnchor u)
drawText x = intoImage (oneLineRect x) (drawOneLine x)



oneLineRect :: (Fractional u, Ord u, FromPtSize u) 
        => Plaintext u -> DrawingR (PlaintextAnchor u)
oneLineRect ptext = 
    monoTextDimensions (text_text ptext) >>= \(w,h) -> 
    return $ PlaintextAnchor $ mkRectangle (0.5*w) (0.5*h) (text_ctm ptext)
                   

drawOneLine :: (Real u, Floating u, FromPtSize u) 
            => Plaintext u -> Graphic u 
drawOneLine (Plaintext { text_text = ss, text_ctm = ctm }) =
    let (ctr,ang) = runShapeGeom ctm ((,) <$> shapeCenter <*> shapeAngle)
    in monoVecToCenter ss >>= \v -> 
       let bl = ctr .-^ v
       in rotTextline ang ss (rotateAbout ang ctr bl)



rotTextline :: (Real u, Floating u) => Radian -> String -> LocGraphic u
rotTextline theta ss baseline_left = 
    withTextAttr $ \rgb attr -> 
        wrapPrim $ rtextlabel rgb attr ss theta baseline_left
