{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Deprecated.PictureLanguage
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Type classes and derived functions to compose 2D /pictures/.
--
-- WARNING - this module is deprecated.
--
--------------------------------------------------------------------------------

module Wumpus.Deprecated.PictureLanguage 
  (
  -- * Data types for alignment 
    HAlign(..)
  , VAlign(..)

  -- * Operations on boundary
  , center

  -- * Composition
  , over 
  , beneath
  , move
  , moveH
  , moveV

  , centerOver
  , nextToH
  , nextToV
  
  , atPoint 
  , centeredAt

  , stackOnto
  , hcat 
  , vcat
  , stackOntoCenter


  , hspace
  , vspace
  , hsep
  , vsep
 
  -- * Compose with alignment
  , alignH
  , alignV
  , hcatA
  , vcatA
  , hsepA
  , vsepA

  ) where

import Wumpus.Core

import Data.AdditiveGroup
import Data.AffineSpace

import Data.List ( foldl' )


--------------------------------------------------------------------------------
-- Data types

-- Alignment

-- | Horizontal alignment - align to the top, center or bottom.
data HAlign = HTop | HCenter | HBottom
  deriving (Eq,Show)

-- | Vertical alignment - align to the left, center or bottom.
data VAlign = VLeft | VCenter | VRight
  deriving (Eq,Show)


--------------------------------------------------------------------------------

-- Operations on bounds

-- $boundsdoc
-- Corresponding operations are available on bounding boxes - the 
-- definitions here have different type class obligations.

-- | The center of a picture.
center :: Fractional u => Picture u -> Point2 u
center = fn . boundary
  where  
    fn (BBox (P2 x0 y0) (P2 x1 y1)) = P2 (x0 + ((x1-x0)*0.5)) 
                                         (y0 + ((y1-y0)*0.5))

rightBound :: Picture u -> u
rightBound = fn . ur_corner . boundary
  where
    fn (P2 x _) = x


leftBound :: Picture u -> u
leftBound = fn . ll_corner . boundary
  where
    fn (P2 x _) = x

bottomBound :: Picture u -> u
bottomBound = fn . ll_corner . boundary
  where
    fn (P2 _ y) = y

topBound :: Picture u -> u
topBound = fn . ur_corner . boundary
  where
    fn (P2 _ y) = y



--------------------------------------------------------------------------------
-- Composition operators

-- | > a `over` b
-- 
-- Place \'picture\' a over b. The idea of @over@ here is in 
-- terms z-ordering, nither picture a or b are actually moved.
--
over    :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
over = picOver


-- | > a `beneath` b
--
-- Similarly @beneath@ draws the first picture behind 
-- the second but move neither.
--

beneath :: (Num u, Ord u) => Picture u -> Picture u -> Picture u

beneath = flip over


  
-- | Move in both the horizontal and vertical.
--
move :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
move dx dy p = p `picMoveBy` (V2 dx dy)


moveH :: (Num u, Ord u) => u -> Picture u -> Picture u
moveH dx p = move dx 0 p

moveV :: (Num u , Ord u) => u -> Picture u -> Picture u
moveV dy p = move 0 dy p






-- | Extract the top-left corner.
--
topleft       :: Picture u -> Point2 u
topleft       = fn . boundary 
  where
    fn (BBox (P2 x0 _) (P2 _ y1)) = P2 x0 y1

-- | Extract the top-right corner.
--
topright      :: Picture u -> Point2 u
topright      = ur_corner . boundary

-- | Extract the bottom-left corner.
--
bottomleft    :: Picture u -> Point2 u
bottomleft    = ll_corner . boundary

-- | Extract the bottom-right corner.
--
bottomright   :: Picture u -> Point2 u
bottomright   = fn . boundary
  where
    fn (BBox (P2 _ y0) (P2 x1 _)) = P2 x1 y0


--------------------------------------------------------------------------------
-- Internal helpers

leftmid       :: Fractional u => Picture u -> Point2 u
leftmid a     = P2 (leftBound a) (midpt (bottomBound a) (topBound a))

rightmid      :: Fractional u => Picture u -> Point2 u
rightmid a    = P2 (rightBound a) (midpt (bottomBound a) (topBound a))


topmid        :: Fractional u => Picture u -> Point2 u
topmid a      = P2 (midpt (leftBound a) (rightBound a)) (topBound a)

bottommid     :: Fractional u => Picture u -> Point2 u
bottommid a   = P2 (midpt (leftBound a) (rightBound a)) (bottomBound a)


midpt :: Fractional a => a -> a -> a
midpt a b = a + 0.5*(b-a)


--------------------------------------------------------------------------------
-- Composition

infixr 5 `nextToV`
infixr 6 `nextToH`, `centerOver`


-- | > a `centerOver` b
-- 
-- Center @a@ on top of @b@, @a@ is potentially moved and drawn 
-- 'over' @b@.
-- 
-- `centerOver` was previously the (-@-) operator.
-- 
centerOver :: (Fractional u, Ord u) => Picture u -> Picture u -> Picture u
p1 `centerOver` p2 = (move x y p1) `over` p2 
  where 
    V2 x y = center p2 .-. center p1


-- | > a `nextToH` b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
-- `nextToH` was previously the (->-) operator.
--
nextToH :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `nextToH` b = a `over` moveH disp b 
  where 
    disp = rightBound a - leftBound b 



-- | > a `nextToV` b
--
-- Vertical composition - move @b@, placing it below @a@.
--
-- nextToV  was previously the (-//-) operator.
--
nextToV :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `nextToV` b = a `over` moveV disp b 
  where 
    disp = bottomBound a - topBound b 


-- | Place the picture at the supplied point.
--
-- `atPoint` was previous the `at` operator.
-- 
atPoint :: (Num u, Ord u) => Picture u -> Point2 u  -> Picture u
p `atPoint` (P2 x y) = move x y p


-- | Center the picture at the supplied point.
--
centeredAt :: (Fractional u, Ord u) => Picture u -> Point2 u ->Picture u
centeredAt p (P2 x y) = move dx dy p 
  where
    bb = boundary p
    dx = x - (boundaryWidth  bb * 0.5)
    dy = y - (boundaryHeight bb * 0.5)



-- | > xs `stackOnto` a
-- 
-- Stack the list of pictures @xs@ 'over' @a@.
--
-- Note, the first picture in the list is drawn at the top, the
-- last picture is draw 'over' @a@.
--
stackOnto :: (Num u, Ord u) => [Picture u] -> Picture u -> Picture u
stackOnto = flip (foldr over)



-- | Concatenate the list pictures @xs@ horizontally with @nextToH@ 
-- starting at @x@.
-- 
hcat :: (Num u, Ord u) => Picture u -> [Picture u] -> Picture u
hcat = foldl' nextToH


-- | Concatenate the list of pictures @xs@ vertically with @nextToV@ 
-- starting at @x@.
--
vcat :: (Num u, Ord u) => Picture u -> [Picture u] -> Picture u
vcat = foldl' nextToV



-- | Stack pictures centered ontop of each other - the first 
-- picture in the list is drawn at the top, last picture is on 
-- drawn at the bottom.
--
stackOntoCenter :: (Fractional u, Ord u) 
                => [Picture u] -> Picture u -> Picture u
stackOntoCenter = flip $ foldr centerOver



--------------------------------------------------------------------------------




-- | > hspace n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
hspace :: (Num u, Ord u) => u -> Picture u -> Picture u -> Picture u
hspace n a b = a `over` moveH (disp + n) b
  where
    disp = rightBound a - leftBound b 

    



-- | > vspace n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
vspace :: (Num u, Ord u) => u -> Picture u -> Picture u -> Picture u
vspace n a b = a `over` moveV (disp + n) b 
  where 
    disp = bottomBound a - topBound b 



-- | > hsep n x xs
--
-- Concatenate the list of pictures @xs@ horizontally with 
-- @hspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
hsep :: (Num u, Ord u) => u -> Picture u -> [Picture u] -> Picture u
hsep n = foldl' (hspace n)



-- | > vsep n x xs
--
-- Concatenate the list of pictures @xs@ vertically with 
-- @vspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
vsep :: (Num u, Ord u) => u -> Picture u -> [Picture u] -> Picture u
vsep n = foldl' (vspace n)


--------------------------------------------------------------------------------
-- Aligning pictures

vecMove :: (Num u, Ord u) => Picture u -> Picture u -> (Vec2 u) -> Picture u 
vecMove a b (V2 x y) = a `over` (move x y) b 


-- | > alignH z a b
--
-- Move picture @b@ up or down to be horizontally aligned along a 
-- line from the top, center or bottom of picture @a@
-- 
alignH :: (Fractional u, Ord u) 
       => HAlign -> Picture u -> Picture u -> Picture u
alignH align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn HTop    = vvec $ topBound p1    - topBound p2
    fn HBottom = vvec $ bottomBound p1 - bottomBound p2
    fn HCenter = vvec v where (V2 _ v) = rightmid p1    .-. leftmid p2


-- | > alignV z a b
--
-- Move picture @b@ left or right to be vertically aligned along a 
-- line from the left side, center or right side of picture @a@
-- 
alignV :: (Fractional u, Ord u) 
       => VAlign -> Picture u -> Picture u -> Picture u
alignV align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn VLeft   = hvec $ leftBound p1  - leftBound p2
    fn VRight  = hvec $ rightBound p1 - rightBound p2
    fn VCenter = hvec h where (V2 h _) = bottommid p1   .-. topmid p2


-- Helpers

-- Unlike alignH this function \"moves and concatenates\".
--
moveAlignH :: (Fractional u, Ord u) 
           =>  HAlign -> Picture u -> Picture u -> Picture u
moveAlignH align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn HTop    = topright p1    .-. topleft p2
    fn HCenter = rightmid p1    .-. leftmid p2
    fn HBottom = bottomright p1 .-. bottomleft p2


-- Unlike alignV this function \"moves and concatenates\".
--
moveAlignV :: (Fractional u, Ord u) 
           => VAlign -> Picture u -> Picture u -> Picture u
moveAlignV align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn VLeft   = bottomleft p1  .-. topleft p2
    fn VCenter = bottommid p1   .-. topmid p2
    fn VRight  = bottomright p1 .-. topright p2



-- Unlike alignH this function \"moves and concatenates\".
--
moveAlignHSep :: (Fractional u, Ord u) 
              =>  HAlign -> u -> Picture u -> Picture u -> Picture u
moveAlignHSep align dx p1 p2 = vecMove p1 p2 $ hvec (-dx) ^+^ fn align
  where
    fn HTop    = topright p1    .-. topleft p2
    fn HCenter = rightmid p1    .-. leftmid p2
    fn HBottom = bottomright p1 .-. bottomleft p2

-- Unlike alignV this function \"moves and concatenates\".
--
moveAlignVSep :: (Fractional u, Ord u) 
           => VAlign -> u -> Picture u -> Picture u -> Picture u
moveAlignVSep align dy p1 p2 = vecMove p1 p2 $ vvec (-dy) ^+^ fn align
  where
    fn VLeft   = bottomleft p1  .-. topleft p2
    fn VCenter = bottommid p1   .-. topmid p2
    fn VRight  = bottomright p1 .-. topright p2



-- | Variant of 'hcat' that aligns the pictures as well as
-- concatenating them.
--
hcatA :: (Fractional u, Ord u) 
      => HAlign -> Picture u -> [Picture u] -> Picture u
hcatA ha = foldl' (moveAlignH ha)

-- | Variant of 'vcat' that aligns the pictures as well as
-- concatenating them.
--
vcatA :: (Fractional u, Ord u) 
      => VAlign -> Picture u -> [Picture u] -> Picture u
vcatA va = foldl' (moveAlignV va)


-- | Variant of @hsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
hsepA :: (Fractional u, Ord u) 
      => HAlign -> u -> Picture u -> [Picture u] -> Picture u
hsepA ha n = foldl' op 
  where 
    a `op` b = moveAlignHSep ha n a b 


-- | Variant of @vsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
vsepA :: (Fractional u, Ord u) 
      => VAlign -> u -> Picture u -> [Picture u] -> Picture u
vsepA va n = foldl' op where 
   a `op` b = moveAlignVSep va n a b 



