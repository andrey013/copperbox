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
    Oscil(..)
  , oscil

  , Terminal(..)
  , terminal
  , linen
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
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- Note sizes should be dependent on fontsize...


-- Fontsize 18 is good for labels eg \"OSCIL\".
-- Parameters, inputs and outputs should be smaller.
--
shapeSty :: DrawingContextF
shapeSty = strokeColour black . thick . fontSize 18 . fontFace helvetica

newtype Oscil u = Oscil { getOscil :: InvSemiellipse u }

type instance DUnit (Oscil u) = u

instance (Real u, Floating u, FromPtSize u) => Outport (Oscil u) where
  outport = south . getOscil

-- instance (Real u, Floating u, FromPtSize u) => Inport2 (Oscil u) where
--   inport2a = 


oscil :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Oscil u)
oscil = glyphCapHeight >>= \ch ->
        mapAns Oscil $ localize shapeSty $ strokedShape $ body ch
  where
    body  = \ch -> setDecoration textF $ invsemiellipse (2.4*ch) (3.6*ch)
    textF = ignoreAns (multiAlignCenter CENTER "OSCIL")




-- Maybe - Csound specific objects should be newtypes over shapes 
-- that have @input@ and @output@ ports rather than anchors?

newtype Terminal u = Terminal { getTerminal :: Circle u }

type instance DUnit (Terminal u) = u


instance (Real u, Floating u, FromPtSize u) => Inport1 (Terminal u) where
  inport1 = north . getTerminal

terminal :: (Real u, Floating u, FromPtSize u) 
         => LocImage u (Terminal u)
terminal = glyphCapHeight >>= \ch -> 
           mapAns Terminal $ localize shapeSty $ strokedShape $ body ch
  where 
    body = \ch -> circle (0.5*ch)



linen :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Trapezium u)
linen = glyphCapHeight >>= \ch -> 
        localize shapeSty $ strokedShape $ body ch
  where
    body  = \ch -> setDecoration textF $ trapezium (8.0*ch) (2.8*ch) ang ang
    textF = ignoreAns (multiAlignCenter CENTER "LINEN")    
    ang   = d2r (72::Double)

-- expon same as linen...
expon :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Trapezium u)
expon = glyphCapHeight >>= \ch -> 
        localize shapeSty $ strokedShape $ body ch
  where
    body  = \ch -> setDecoration textF $ trapezium (8.0*ch) (2.8*ch) ang ang
    textF = ignoreAns (multiAlignCenter CENTER "EXPON")
    ang   = d2r (72::Double)

-- Can have a stdTrapezium function...

--------------------------------------------------------------------------------


newtype Buzz u = Buzz { getBuzz :: Rectangle u }

type instance DUnit (Buzz u) = u


buzz :: (Real u, Floating u, FromPtSize u) 
     => LocImage u (Buzz u)
buzz = glyphCapHeight >>= \ch ->
       mapAns Buzz $ localize shapeSty $ strokedShape $ body ch
  where
    body  = \ch -> setDecoration textF $ rectangle (8.0*ch) (2.8*ch)
    textF = ignoreAns (multiAlignCenter CENTER "BUZZ")


instance (Real u, Floating u, FromPtSize u) => Outport (Buzz u) where
  outport = south . getBuzz

-- needs \"+\" for body
adder :: (Real u, Floating u, FromPtSize u) 
      => LocImage u (Circle u)
adder = localize shapeSty $ strokedShape $ circle 10

-- needs \"x\" for body
multiplier :: (Real u, Floating u, FromPtSize u) 
           => LocImage u (Circle u)
multiplier = localize shapeSty $ strokedShape $ circle 10

