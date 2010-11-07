{-# LANGUAGE TypeFamilies               #-}
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

  , AdvanceSingle
  , AdvanceMulti

  , runAdvanceMulti 

  , makeSingle
  
  , advanceR
  , oneLineH
  , alignRightH
  , alignLeftH
  , alignCenterH

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Text.LocBoundingBox


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace



newtype AdvanceVec u = AdvanceVec { getAdvanceVec :: Vec2 u }
  deriving (Eq,Show)

type instance DUnit (AdvanceVec u) = u

appendAdvanceVec :: Num u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
appendAdvanceVec a b = AdvanceVec $ getAdvanceVec a ^+^ getAdvanceVec b

-- | Take the max width and set the height to zero.
--
-- It is assumed that any deviation from zero in the height
-- component represents that the end vector is in super- or 
-- sub-script mode. As 'advanceHMax' is used in multi-line 
-- concatenation, losing the mode seems acceptable.
--
advanceH :: Num u => AdvanceVec u -> Vec2 u
advanceH (AdvanceVec (V2 w _))  = V2 w 0




-- Whoa - there\'s a lot in favour of having /Line/ and 
-- /MultiLine/ at different types.


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

runAdvanceMulti :: Point2 u -> AdvanceMulti u -> Image u (BoundingBox u)
runAdvanceMulti p0 (AdvanceMulti bbox _ gf) = 
    intoImage (wrap $ runLocBoundingBox p0 bbox) (gf `at` p0)


vcombine :: Num u 
         => LocGraphic u -> Vec2 u -> LocGraphic u -> LocGraphic u
vcombine a v b = promote1 $ \p0 -> 
    let p1   = p0 .+^ v  in (a `at` p0) `oplus` (b `at` p1)

makeSingle :: LocBoundingBox u -> Vec2 u -> LocGraphic u -> AdvanceSingle u
makeSingle bbox v gf = AdvanceSingle bbox (AdvanceVec v) gf

-- | Place the second TextPath at the end of the first.
--
advanceR :: (Num u, Ord u) 
        => AdvanceSingle u -> AdvanceSingle u -> AdvanceSingle u 
advanceR a b = AdvanceSingle bbox adv grafic
  where
    vmove   = getAdvanceVec $ single_adv_vec a
    bbox    = shiftUnion (single_bbox a) vmove (single_bbox b)
    adv     = single_adv_vec a `appendAdvanceVec` single_adv_vec b
    grafic  = vcombine (single_graphic a) vmove (single_graphic b)


oneLineH :: Num u => AdvanceSingle u -> AdvanceMulti u
oneLineH (AdvanceSingle bbox adv gf) = AdvanceMulti bbox (advanceH adv) gf


alignRightH :: (Num u, Ord u) 
            => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignRightH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 xa ya  = multi_dimension a
    V2 xb yb  = multi_dimension b
    vmove     = vec (negate xb) (negate $ dy + ya)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max xa xb) (dy + ya + yb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  


alignLeftH :: (Num u, Ord u) 
           => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignLeftH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 xa ya  = multi_dimension a
    V2 xb yb  = multi_dimension b
    vmove     = vvec (negate $ dy + ya)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max xa xb) (dy + ya + yb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  


alignCenterH :: (Fractional u, Ord u) 
             => u -> AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u
alignCenterH dy a b = AdvanceMulti bbox dimm grafic
  where
    V2 xa ya  = multi_dimension a
    V2 xb yb  = multi_dimension b
    vmove     = vec (negate $ (0.5*xa) + (0.5*xb)) (negate $ dy + ya)
    bbox      = shiftUnion (multi_bbox a) vmove (multi_bbox b)
    dimm      = V2 (max xa xb) (dy + ya + yb)
    grafic    = vcombine (multi_graphic a) vmove (multi_graphic b)  

