{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.Advance
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Building text with advance vectors and paths.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.Advance
  ( 

    AdvanceVec
  , advanceH
  
  , AdvanceSingle
  , AdvanceMulti

  , runAdvanceMulti 

  , makeSingle
  
  , advanceR
  , oneLineH
  , alignRightH
  , alignLeftH
  , alignCenterH

  , singleLine

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Text.LocBoundingBox


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Char
import Data.Foldable ( foldrM )


type AdvanceVec u = Vec2 u

-- | Take the max width and set the height to zero.
--
-- It is assumed that any deviation from zero in the height
-- component represents that the end vector is in super- or 
-- sub-script mode. As 'advanceHMax' is used in multi-line 
-- concatenation, losing the mode seems acceptable.
--
advanceH :: Num u => AdvanceVec u -> Vec2 u
advanceH (V2 w _)  = V2 w 0



data AdvanceSingle u = AdvanceSingle
       { single_bbox        :: LocBoundingBox u
       , single_adv_vec     :: AdvanceVec u
       , single_graphic     :: LocGraphic u
       }

data AdvanceMulti u = AdvanceMulti 
       { multi_bbox         :: LocBoundingBox u
       , multi_dimension    :: Vec2 u
       , multi_graphic      :: LocGraphic u
       }       

runAdvanceMulti :: AdvanceMulti u -> LocImage u (BoundingBox u)
runAdvanceMulti (AdvanceMulti bbox _ gf) = 
    intoLocImage (promote1 $ \pt -> wrap $ runLocBoundingBox pt bbox) (gf)


vcombine :: Num u 
         => LocGraphic u -> Vec2 u -> LocGraphic u -> LocGraphic u
vcombine a v b = promote1 $ \p0 -> 
    let p1   = p0 .+^ v  in (a `at` p0) `oplus` (b `at` p1)

makeSingle :: LocBoundingBox u -> Vec2 u -> LocGraphic u -> AdvanceSingle u
makeSingle bbox v gf = AdvanceSingle bbox v gf

-- | Place the second TextPath at the end of the first.
--
advanceR :: (Num u, Ord u) 
        => AdvanceSingle u -> AdvanceSingle u -> AdvanceSingle u 
advanceR a b = AdvanceSingle bbox adv grafic
  where
    vmove   = single_adv_vec a
    bbox    = shiftUnion (single_bbox a) vmove (single_bbox b)
    adv     = single_adv_vec a ^+^ single_adv_vec b
    grafic  = vcombine (single_graphic a) vmove (single_graphic b)


oneLineH :: Num u => AdvanceSingle u -> AdvanceMulti u
oneLineH (AdvanceSingle bbox adv gf) = AdvanceMulti bbox (advanceH adv) gf


alignRightH :: (Num u, Ord u) 
            => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignRightH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 wa ha  = multi_dimension a
    V2 wb hb  = multi_dimension b
    vmove     = vec (wa - wb) (negate $ dy + ha)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max wa wb) (dy + ha + hb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  


alignLeftH :: (Num u, Ord u) 
           => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignLeftH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 wa ha  = multi_dimension a
    V2 wb hb  = multi_dimension b
    vmove     = vvec (negate $ dy + ha)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max wa wb) (dy + ha + hb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  


alignCenterH :: (Fractional u, Ord u) 
             => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignCenterH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 wa ha  = multi_dimension a
    V2 wb hb  = multi_dimension b
    vmove     = vec (0.5 * (wa-wb)) (negate $ dy + ha)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max wa wb) (dy + ha + hb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  


--------------------------------------------------------------------------------


singleLine :: FromPtSize u => String -> CF (AdvanceSingle u)
singleLine ss = 
    stringVector ss >>= \av -> 
    maxGlyphHeight  >>= \max_h ->
    let width = vector_x av
        bbox  = oLocBoundingBox width max_h
    in  return  (makeSingle bbox av (textline ss))


-- TODO - this should account for escape characters...
--
stringVector :: FromPtSize  u =>  String -> CF (AdvanceVec u)
stringVector ss = 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) ss



charVector :: FromPtSize u => Char -> CF (AdvanceVec u)
charVector c = avLookupTable `situ1` (ord c)
