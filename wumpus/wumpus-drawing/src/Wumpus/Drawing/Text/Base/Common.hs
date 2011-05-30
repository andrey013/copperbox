{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.Common
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for working with measured / advance text.
-- 
-- This module not expected to be used in client code.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.Common
  ( 

    TextVSize(..)
  , posTextWithMargins
  , advtext
  , textVector
  , textOrientationZero

  , charVector
  , charOrientationZero
  , hkernVector
  , hkernOrientationZero

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- | Wumpus distinguishes two use-cases for displaying vertically 
-- centered text.
-- 
-- Arbitrary text that is expected to contain lower case letters 
-- with descenders, show take the vertical center as the mid-point 
-- between the cap height and the descender depth.
--
-- Unfortunately including the descender depth can produce 
-- unbalanced results for text which is not expected to have 
-- descenders - e.g. numbers within an bordered box. Including the 
-- descender depth in this case makes the center visually too high.
-- 
data TextVSize = JUST_CAP_HEIGHT | CAP_HEIGHT_PLUS_DESCENDER
  deriving (Enum,Eq,Ord,Show)



posTextWithMargins :: (Fractional u, InterpretUnit u) 
                   => PosObject u -> (RectAddress -> LocImage u (BoundingBox u))
posTextWithMargins obj = \raddr ->
    textMargin >>= \(xsep,ysep) -> 
    let body = extendPosObject xsep xsep ysep ysep obj
    in runPosObject raddr body


--------------------------------------------------------------------------------
-- Produce advance vectors.

-- | Single line text, returning its advance vector.
--
advtext :: InterpretUnit u => EscapedText -> LocImage u (Vec2 u)
advtext esc = textVector esc >>= body
  where
    body v = replaceAns v $ dcEscapedlabel esc


-- | Find the advance vector for the supplied 'EscapedText'.
--
textVector :: (DrawingCtxM m, InterpretUnit u) 
           => EscapedText -> m (AdvanceVec u)
textVector esc = 
    cwLookupTable >>= \table -> 
    pointSize     >>= \sz    -> 
    let cs = destrEscapedText id esc 
    in return $ foldr (step sz table) (vec 0 0) cs
  where
    step sz table ch v = (v ^+^) $ charWidth sz table ch


-- | Find the advance vector for the supplied 'EscapedChar'.
--
charVector :: (DrawingCtxM m, InterpretUnit u) 
           => EscapedChar -> m (AdvanceVec u)
charVector ch = 
    (\table sz -> charWidth sz table ch) <$> cwLookupTable <*> pointSize



-- | This is outside the Drawing context as we don\'t want to get
-- the @cwLookupTable@ for every char.
--
charWidth :: InterpretUnit u 
          => FontSize -> CharWidthLookup -> EscapedChar -> AdvanceVec u
charWidth sz fn (CharLiteral c) = fmap (dinterp sz) $ fn $ ord c
charWidth sz fn (CharEscInt i)  = fmap (dinterp sz) $ fn i
charWidth sz fn (CharEscName s) = fmap (dinterp sz) $ fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices



-- | 'hkernVector' : @ [kerning_char] -> AdvanceVec @
-- 
-- 'hkernvector' takes whatever length is paired with the 
-- EscapedChar for the init of the the list, for the last element 
-- it takes the charVector.
--
hkernVector :: (DrawingCtxM m, InterpretUnit u) 
            => [KernChar u] -> m (AdvanceVec u)
hkernVector = go 0
  where
    go w []             = return $ V2 w 0
    go w [(dx,ch)]      = fmap (addWidth $ w + dx) (charVector ch)
    go w ((dx,_ ):xs)   = go (w + dx) xs
    
    addWidth w (V2 x y) = V2 (w+x) y

    -- ERROR this not the right way to count...


--------------------------------------------------------------------------------


-- | Build the Orientation of a single line of EscapedText.
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
textOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                    => TextVSize -> EscapedText -> m (Orientation u)
textOrientationZero vsz esc = textVector esc >>= bllOrientationZero vsz

-- | Build the Orientation of an EscapedChar.
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
charOrientationZero :: (DrawingCtxM m, InterpretUnit u)
                    => TextVSize -> EscapedChar -> m (Orientation u)
charOrientationZero vsz ch = charVector ch >>= bllOrientationZero vsz



-- NOTE - TextMargin should probably be added as a final step
-- not during construction...

bllOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                   => TextVSize -> AdvanceVec u -> m (Orientation u)
bllOrientationZero JUST_CAP_HEIGHT (V2 w _) = 
    (\ymaj -> Orientation 0 w 0 ymaj) <$> capHeight

bllOrientationZero CAP_HEIGHT_PLUS_DESCENDER (V2 w _) = 
    (\ymin ymaj -> Orientation 0 w (abs ymin) ymaj) <$> descender <*> capHeight



-- TextVSize = JUST_CAP_HEIGHT | CAP_HEIGHT_PLUS_DESCENDER


   
    

hkernOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                     => TextVSize -> [KernChar u] -> m (Orientation u)
hkernOrientationZero vsz xs = hkernVector xs >>= bllOrientationZero vsz
 

