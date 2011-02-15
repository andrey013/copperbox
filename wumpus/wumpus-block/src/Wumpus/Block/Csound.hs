{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Block.Csound
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Block diagram shapes specifically tailored to diagrams in the
-- Csound book.
--
--------------------------------------------------------------------------------

module Wumpus.Block.Csound
  ( 

    labelOutport1
  , minorFontSize 

  , Oscil(..)
  , oscil
  , zoscil

  , scaleFactor

  , Terminal(..)
  , terminal

  , Linen(..)
  , linen

  , Expon(..)
  , expon

  , Buzz(..)
  , buzz
  , adder
  , multiplier

  ) where

import Wumpus.Block.Base

-- package: wumpus-drawing
import Wumpus.Drawing.Colour.SVGColours hiding ( linen )
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Ratio



stripTheta :: LocCF u a -> LocThetaCF u a
stripTheta mf = promoteR2 $ \pt _ -> mf `at` pt

-- Note sizes should be dependent on fontsize...

--
-- Measurements primarily derived from figures 20.26, 20.28 and 
-- 20.29 in Csound book.
--
-- Shape sizes seem to vary in the Csound book, however in 
-- Wumpus-Block it is preferred to make them more uniform.

-- Temporarily - Fontsize 18 is good for labels eg \"OSCIL\".
-- Parameters, inputs and outputs should be smaller.
--
shapeSty :: DrawingContextF
shapeSty = stroke_colour black . line_thick . point_size 18 . set_font helvetica




scaleFactor :: (Fractional u,  FromPtSize u, DrawingCtxM m) => m u
scaleFactor = (\a -> 0.2855 * (fromPtSize 1) * (fromIntegral a)) 
               <$> getFontSize



singleMajorText :: (Real u, Floating u, FromPtSize u) 
                => String -> Shape u a -> Shape u a
singleMajorText name =
    setDecoration (lift1R2 $ ignoreAns $ singleLine name `startPos` CENTER)



--------------------------------------------------------------------------------

-- Note - this needs rendring with @draw@ in the user code, but 
-- the type signature is horrible if @draw@ is put in the 
-- function...
--

labelOutport1 :: ( Outport1 a
                 , Real u, Floating u, FromPtSize u
                 , u ~ DUnit a ) 
              => a -> String -> Graphic u
labelOutport1 a msg = 
    localize minorFontSize $
       ignoreAns $ (singleLine msg `startPos` NE) `at` outport1 a

{-
-- Ideally, with a better name this would be valuable in 
-- Wumpus-basic...
--
thetaHorizontal :: LocThetaCF u a -> LocCF u a
thetaHorizontal a = a `rot` 0  
-}


minorFontSize :: DrawingContextF
minorFontSize = scalePointSize (4%5)


-- Inports should be evenly spaced along the top line (measured 
-- at 4mm apart on the sample diagrams).
--
-- @toplineAnchor@ needs scaling factor, but it also needs to be
-- /context free/ i.e. not in the CF monad because anchors are 
-- extracted by bind.
--
-- Thus Csound objects (oscil, buzz, etc.) need to be a pair of 
-- (shape, scale_factor) so scale_factor can be used to calculate 
-- the position of inports.
--

data CsoundObj sh u = CsoundObj 
      { cs_obj_shape    :: sh u
      , cs_obj_scaling  :: u
      }

type instance DUnit (CsoundObj sh u) = u

 

toplineAnchor :: (CardinalAnchor (sh u), Fractional u, u ~ DUnit (sh u)) 
              => Int -> CsoundObj sh u -> Point2 u
toplineAnchor n obj = displaceH hunit (north $ cs_obj_shape obj)
  where
    hunit = 4.0 * cs_obj_scaling obj * fromIntegral n


--------------------------------------------------------------------------------

-- Need to wrap a shape with the scale factor to create topline 
-- anchors...
--


newtype Oscil u = Oscil { getOscil :: CsoundObj InvSemiellipse u  }

type instance DUnit (Oscil u) = u

instance (Real u, Floating u, FromPtSize u) => Outport1 (Oscil u) where
  outport1 = south . cs_obj_shape . getOscil

instance (Real u, Floating u, FromPtSize u) => Inport2 (Oscil u) where
  inport2a (Oscil a) = toplineAnchor (-1) a
  inport2b (Oscil a) = toplineAnchor   1  a

--
-- Note - the inports on an Oscil have no DrawingContext to get
-- @scaleFactor@. The positions: 
-- 
-- > a = displaceH (negate $ 3*sc)
-- > a = displaceH (3*sc)
-- 
-- ... must be calculable from other anchors rather than via the
-- @scaleFactor@
--
-- So we need proper corner anchors not just cardinals.
--


oscil :: (Real u, Floating u, FromPtSize u) 
      => String -> LocImage u (Oscil u)
oscil name = 
    scaleFactor    >>= \sc ->
    mapAns (mk sc) $ localize shapeSty $ strokedShape $ body sc
  where
    body  = \sc -> setDecoration deco $ invsemiellipse (6*sc) (9*sc)
    deco  = stripTheta $ oscilText name 

    mk sc sh = Oscil (CsoundObj sh sc)

-- Design note - instead of this formulation, maybe 
-- /function number/ is another anchor on shapes like 
-- @outport1@...
--
zoscil :: (Real u, Floating u, FromPtSize u) 
       => LocImage u (Oscil u)
zoscil = oscil ""


oscilText :: (Real u, Floating u, FromPtSize u) 
          => String -> LocGraphic u
oscilText name = scaleFactor >>= \sc -> 
                 major sc `oplus` minor sc
  where
    major = \sc -> ignoreAns $ disp 1 sc (singleLine "OSCIL" `startPos` CENTER)
    minor = \sc -> localize minorFontSize $
                     ignoreAns $ disp (-2) sc (singleLine name `startPos` CENTER)


    disp d = \sc -> moveStart (displaceV $ d*sc)



--------------------------------------------------------------------------------

-- Maybe - Csound specific objects should be newtypes over shapes 
-- that have @input@ and @output@ ports rather than anchors?

newtype Terminal u = Terminal { getTerminal :: CsoundObj Circle u }

type instance DUnit (Terminal u) = u


instance (Real u, Floating u, FromPtSize u) => Inport1 (Terminal u) where
  inport1 = north . cs_obj_shape . getTerminal

terminal :: (Real u, Floating u, FromPtSize u) 
         => LocImage u (Terminal u)
terminal = scaleFactor >>= \sc -> 
           mapAns (mk sc) $ localize shapeSty $ strokedShape $ body sc
  where 
    body = \sc -> circle (1.75*sc)
    mk sc sh = Terminal (CsoundObj sh sc)


--------------------------------------------------------------------------------

newtype Linen u = Linen { getLinen :: CsoundObj Trapezium u } 

type instance DUnit (Linen u) = u

instance (Real u, Floating u, FromPtSize u) => Outport1 (Linen u) where
  outport1 = south . cs_obj_shape . getLinen

linen :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Linen u)
linen = scaleFactor >>= \sc -> 
    mapAns (mk sc) $ stdTrapezium (singleMajorText "LINEN")
  where 
    mk sc sh = Linen (CsoundObj sh sc)


--------------------------------------------------------------------------------

-- expon same as linen...

newtype Expon u = Expon { getExpon :: Trapezium u }

type instance DUnit (Expon u) = u

instance (Real u, Floating u, FromPtSize u) => Outport1 (Expon u) where
  outport1 = south . getExpon

expon :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Expon u)
expon = mapAns Expon $ stdTrapezium (singleMajorText "EXPON")




stdTrapezium :: (Real u, Floating u, FromPtSize u) 
             => (Shape u (Trapezium u) -> Shape u (Trapezium u))
             -> LocImage u (Trapezium u)
stdTrapezium fn = scaleFactor >>= \sc -> 
        localize shapeSty $ strokedShape $ fn $ body sc
  where
    body  = \sc -> trapezium (20*sc) (7*sc) ang ang
    ang   = d2r (72::Double)


--------------------------------------------------------------------------------


newtype Buzz u = Buzz { getBuzz :: Rectangle u }

type instance DUnit (Buzz u) = u


buzz :: (Real u, Floating u, FromPtSize u) 
     => LocImage u (Buzz u)
buzz = scaleFactor >>= \sf ->
       mapAns Buzz $ localize shapeSty $ strokedShape $ body sf
  where
    body  = \sf -> setDecoration textF $ rectangle (20*sf) (7*sf)
    textF = lift1R2 $ ignoreAns (apply2R3 (multiAlignCenter "BUZZ") CENTER 0)


instance (Real u, Floating u, FromPtSize u) => Outport1 (Buzz u) where
  outport1 = south . getBuzz

-- needs \"+\" for body
adder :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Circle u)
adder = localize shapeSty $ strokedShape $ circle 10

-- needs \"x\" for body
multiplier :: (Real u, Floating u, FromPtSize u) 
           => LocImage u (Circle u)
multiplier = localize shapeSty $ strokedShape $ circle 10

