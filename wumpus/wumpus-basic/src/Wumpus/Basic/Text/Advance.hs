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
  , AdvancePath
  , TextPath

  , textLocGraphic
  , advance

  , endAlignLR
  , startAlignLR
  , centerAlignLR

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
-- sub-script mode. As 'advanceHMax' is used to make the next 
-- line, losing the mode seems acceptable.
--
advanceHMax :: (Num u, Ord u) 
            => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
advanceHMax (AdvanceVec (V2 w1 _)) (AdvanceVec (V2 w2 _)) = 
    AdvanceVec $ hvec (max w1 w2)


-- | Halve the horizontal component of the vector.
--
-- It is assumed that any deviation from zero in the height
-- component represents that the end vector is in super- or 
-- sub-script mode. Losing the mode seems acceptable.
--
halfH :: Fractional u => AdvanceVec u -> u
halfH (AdvanceVec (V2 w1 _)) = 0.5*w1



-- | v is some measurement - generally a bounding box
--   u is the unit of the drawing (usually Double represeting a PS point).
--   a is the element - for text this is a LocGraphic.
--
data AdvancePath u v a = 
          Single v                    -- annotated measure - usually BBox
                 (AdvanceVec u)       -- dimensions as a vector
                 a                    -- element - usually a LocGraphic

        | Join   v                    -- annotated measure - usually BBox 
                 (AdvanceVec u)       -- dimensions as a vector
                 (AdvancePath u v a)  -- left-hand-side
                 (AdvanceVec u)       -- displacement from lhs-end to rhs-start
                 (AdvancePath u v a)  -- right-hand-side
  deriving (Eq,Show)

type TextPath u = AdvancePath u (LocBoundingBox u) (LocGraphic u)



endVector :: AdvancePath u v a -> Vec2 u
endVector (Single _ wh  _)  = getAdvanceVec wh 
endVector (Join _ wh _ _ _) = getAdvanceVec wh 

-- For wumpus-core
vreverse :: Num u => Vec2 u -> Vec2 u
vreverse (V2 x y) = V2 (negate x) (negate y) 


pathMeasure :: AdvancePath u v a -> v
pathMeasure (Single v _ _)    = v
pathMeasure (Join v _ _ _ _)  = v

pathDimensions :: AdvancePath u v a -> AdvanceVec u
pathDimensions (Single _ d _)    = d
pathDimensions (Join _ d _ _ _)  = d



textLocGraphic :: Num u => TextPath u -> LocGraphic u
textLocGraphic (Single _ _ a) = a
textLocGraphic (Join _ _ l v r) = promote1 $ \p0 -> 
    let end1 = endVector l
        p1   = p0 .+^ (end1 ^+^ getAdvanceVec v)
    in (textLocGraphic l `at` p0) `oplus` (textLocGraphic r `at` p1)

-- Concatenation operators are not associative.

-- | Place the second TextPath at the end of the first.
--
advance :: (Num u, Ord u) => TextPath u -> TextPath u -> TextPath u 
advance a b = Join bbox dimv a nomove b
  where
    bbox    = shiftUnion (pathMeasure a) (endVector a) (pathMeasure b)
    dimv    = pathDimensions a `appendAdvanceVec` pathDimensions b
    nomove  = AdvanceVec $ vec 0 0 

-- Whoa, this looks wrong...
-- It is only moving one line regardless of the height of @a@.
--
endAlignLR :: (Num u, Ord u) => u -> TextPath u -> TextPath u -> TextPath u
endAlignLR dy a b = Join bbox dimv a move b
  where
    vmove   = vreverse (endVector b) ^+^ vvec (-dy)
    bbox    = shiftUnion (pathMeasure a) vmove (pathMeasure b)
    dimv    = advanceHMax (pathDimensions a) (pathDimensions b)
    move    = AdvanceVec vmove  


-- Whoa, this looks wrong...
-- It is only moving one line regardless of the height of @a@.
--
startAlignLR :: (Num u, Ord u) => u -> TextPath u -> TextPath u -> TextPath u
startAlignLR dy a b = Join bbox dimv a move b
  where
    vmove   = vvec (-dy)
    bbox    = shiftUnion (pathMeasure a) vmove (pathMeasure b)
    dimv    = advanceHMax (pathDimensions a) (pathDimensions b)
    move    = AdvanceVec vmove  


centerAlignLR :: (Fractional u, Ord u) 
              => u -> TextPath u -> TextPath u -> TextPath u
centerAlignLR dy a b = Join bbox dimv a move b
  where
    dx      = halfH (pathDimensions a) + halfH (pathDimensions b)
    vmove   = vec (-dx) (-dy)
    bbox    = shiftUnion (pathMeasure a) vmove (pathMeasure b)
    dimv    = advanceHMax (pathDimensions a) (pathDimensions b)
    move    = AdvanceVec vmove  
