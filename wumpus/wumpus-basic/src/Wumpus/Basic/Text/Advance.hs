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

  , singleLineLR

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Text.LocBoundingBox

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Char
import Data.Foldable ( foldrM )
import qualified Data.Map               as Map
import Data.Maybe 

-- Note - LocImage u (BoundingBox u) needs a name.
-- This type also enjoys composition operators along the lines
-- of DrawingComposition.


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
oneLineH (AdvanceSingle bbox adv gf) = AdvanceMulti bbox (hvec $ advanceH adv) gf


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


singleLineLR :: FromPtSize u => String -> CF (AdvanceSingle u)
singleLineLR ss = 
    let cs = escapeString ss in 
    stringVector cs >>= \av -> 
    glyphHeightRange  >>= \(ymin,ymax) ->
    let width = vector_x av
        bbox  = oLocBoundingBox width (ymax-ymin)
    in  return  (makeSingle bbox av (escapedline cs))


stringVector :: FromPtSize  u => EscapedText -> CF (AdvanceVec u)
stringVector ss = let cs = getEscapedText ss in 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) cs


charVector :: FromPtSize u => EscapedChar -> CF (AdvanceVec u)
charVector (CharLiteral c) = unCF1 (ord c) avLookupTable
charVector (CharEscInt i)  = unCF1 i       avLookupTable
charVector (CharEscName s) = unCF1 ix      avLookupTable
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices


