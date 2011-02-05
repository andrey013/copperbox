{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for working with measured / advance text.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base
  ( 

    advtext
  , textVector
  , charVector

  , centerToBaseline
  , centerSpinePoints

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- | Single line text, returning its advance vector.
--
advtext :: FromPtSize u => EscapedText -> AdvGraphic u
advtext esc = lift0R1 (textVector esc) >>= \v -> replaceAns v $ escapedline esc


-- kernchar versions to do...


textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = 
    cwLookupTable >>= \table -> 
       let cs = destrEscapedText id esc 
       in return $ foldr (\c v -> v ^+^ (charWidth table c)) (vec 0 0) cs
     
charVector :: FromPtSize u => EscapedChar -> DrawingInfo (AdvanceVec u)
charVector ch = cwLookupTable >>= \table -> return $ charWidth table ch
   

-- | This is outside the Drawing context as we don\'t want to get
-- the @cwLookupTable@ for every char.
--
charWidth :: FromPtSize u 
          => CharWidthTable u -> EscapedChar -> AdvanceVec u
charWidth fn (CharLiteral c) = fn $ ord c
charWidth fn (CharEscInt i)  = fn i
charWidth fn (CharEscName s) = fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices


--------------------------------------------------------------------------------
-- start points for multiline text

-- | Calculate the distance from the center of a one-line textbox 
-- to the baseline. Note the height of a textbox is @vspan@ which 
-- is cap_height + descender
--
centerToBaseline :: (Fractional u, FromPtSize u) => DrawingInfo u
centerToBaseline = 
    (\ch vspan -> ch - 0.5 * vspan) <$> glyphCapHeight <*> glyphVerticalSpan

-- | All drawing is done on a spine that plots points from the 
-- first line of text. The spine is calculated from its center and
-- has to account for the inclination.
--
centerSpinePoints :: Floating u 
                  => Int -> Radian -> DrawingInfo (LocChain u)
centerSpinePoints n theta
    | n <= 1    = return (\pt -> [pt])
    | otherwise = baselineSpacing >>= \h1 -> 
                  let dist_top = h1 * centerCount n
                      mktop    = \ctr -> thetaNorthwards dist_top theta ctr    
                  in return (\ctr -> take n $ iterate (thetaSouthwards h1 theta) 
                                                      (mktop ctr))




-- | Count the steps from the center to an end:
--
-- >
-- > 1 = 0     .
-- > 2 = 0.5   .__.
-- > 3 = 1     .__.__.
-- > 4 = 1.5   .__.__.__.
-- >
--
centerCount :: Fractional u => Int -> u
centerCount i = (fromIntegral i) / 2 - 0.5
