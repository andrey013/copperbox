{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.PictureLanguage
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Composition operators for Pictures.
--
-- Note - empty pictures cannot exist in Wumpus hence the /list/ 
-- functions in this module are always supplied with an initial 
-- picture, as well as the (possibly empty) list.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.PictureLanguage 
  (
  -- * Data types for alignment 
    HAlign(..)
  , VAlign(..)

  -- * Operations on boundary
  , centerPoint

  -- * Composition
  , over 
  , under

  , centerOver
  , nextToH
  , nextToV
  
  , atPoint 
  , centeredAt

  , stackOver
  , zconcat

  , hcat 
  , vcat
  , stackOverCenter


  , hspace
  , vspace
  , hsep
  , vsep
 
  -- * Compose with alignment
  , alignH
  , alignV
  , alignHSep
  , alignVSep
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

-- Corresponding operations are available on bounding boxes - the 
-- definitions here have different type class obligations.

-- | The center of a picture.
centerPoint :: Fractional u => Picture u -> Point2 u
centerPoint = fn . boundary
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


-- | > a `under` b
--
-- Similarly @under@ draws the first picture behind 
-- the second but move neither.
--
-- @under@ was previously @beneath@.
--
under :: (Num u, Ord u) => Picture u -> Picture u -> Picture u

under = flip over


  
-- | Move in both the horizontal and vertical.
--
move :: (Num u, Ord u) => Vec2 u -> Picture u -> Picture u
move = flip picMoveBy







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


-- Note - `centerOver` moves the first argument, whereas other 
-- functions move the second...

-- | Draw a centered over b - a is moved, b is static.
--
-- > a `centerOver` b 
--
-- 'centerOver' was previously the (-\@-) operator.
-- 
centerOver :: (Fractional u, Ord u) => Picture u -> Picture u -> Picture u
p1 `centerOver` p2 = (move v p1) `over` p2 
  where 
    v = centerPoint p2 .-. centerPoint p1


-- | > a `nextToH` b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
-- 'nextToH' was previously the (->-) operator.
--
nextToH :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `nextToH` b = a `over` move hv b 
  where 
    hv = hvec $ rightBound a - leftBound b 



-- | > a `nextToV` b
--
-- Vertical composition - move @b@, placing it below @a@.
--
-- nextToV  was previously the (-//-) operator.
--
nextToV :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `nextToV` b = a `over` move vv b 
  where 
    vv = vvec $ bottomBound a - topBound b 


-- | Place the picture at the supplied point.
--
-- `atPoint` was previous the `at` operator.
-- 
atPoint :: (Num u, Ord u) => Picture u -> Point2 u  -> Picture u
p `atPoint` (P2 x y) = move (V2 x y) p


-- | Center the picture at the supplied point.
--
centeredAt :: (Fractional u, Ord u) => Picture u -> Point2 u ->Picture u
centeredAt p (P2 x y) = move (vec dx dy) p 
  where
    bb = boundary p
    dx = x - (boundaryWidth  bb * 0.5)
    dy = y - (boundaryHeight bb * 0.5)



-- | > xs `stackOver` x
-- 
-- Stack the list of pictures @xs@ 'over' @x@.
--
-- Note, the first picture in the list is drawn at the top, all 
-- the pictures in the list are drawn \'over\' @x@. No pictures
-- are moved 
--
-- @ [p1,p2,p3] `stackOver` p4 => [p1,p2,p3,p4] @
--
stackOver :: (Num u, Ord u) => [Picture u] -> Picture u -> Picture u
stackOver = flip (foldr over)



-- | > x `zconcat` xs
-- 
-- Concatenate @x@ over the list of pictures @xs@. 
--
-- @x@ is drawn at the top. No pictures are moved. 
--
-- @ p1 `zconcat` [p2,p3,p4] => [p1,p2,p3,p4] @
--
zconcat :: (Num u, Ord u) => Picture u -> [Picture u] -> Picture u
zconcat = foldl' over





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
stackOverCenter :: (Fractional u, Ord u) 
                => [Picture u] -> Picture u -> Picture u
stackOverCenter = flip $ foldr centerOver



--------------------------------------------------------------------------------




-- | > hspace n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
hspace :: (Num u, Ord u) => u -> Picture u -> Picture u -> Picture u
hspace n a b = a `over` move hv b
  where
    hv = hvec $ n + rightBound a - leftBound b 

    



-- | > vspace n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
vspace :: (Num u, Ord u) => u -> Picture u -> Picture u -> Picture u
vspace n a b = a `over` move vv b 
  where 
    vv = vvec $ bottomBound a - topBound b - n



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
vecMove a b v = a `over` (move v b)



-- | > alignH align a b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ and align it with the top, center or bottom of @a@.
-- 
alignH :: (Fractional u, Ord u) 
       =>  HAlign -> Picture u -> Picture u -> Picture u
alignH align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn HTop    = topright p1    .-. topleft p2
    fn HCenter = rightmid p1    .-. leftmid p2
    fn HBottom = bottomright p1 .-. bottomleft p2


-- | > alignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
alignV :: (Fractional u, Ord u) 
           => VAlign -> Picture u -> Picture u -> Picture u
alignV align p1 p2 = vecMove p1 p2 $ fn align
  where
    fn VLeft   = bottomleft p1  .-. topleft p2
    fn VCenter = bottommid p1   .-. topmid p2
    fn VRight  = bottomright p1 .-. topright p2



-- | > alignHSep align sep a b
-- 
-- Spacing version of alignH - move @b@ to the right of @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignHSep :: (Fractional u, Ord u) 
              =>  HAlign -> u -> Picture u -> Picture u -> Picture u
alignHSep align dx p1 p2 = vecMove p1 p2 $ hvec dx ^+^ fn align
  where
    fn HTop    = topright p1    .-. topleft p2
    fn HCenter = rightmid p1    .-. leftmid p2
    fn HBottom = bottomright p1 .-. bottomleft p2

-- | > alignHSep align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignVSep :: (Fractional u, Ord u) 
           => VAlign -> u -> Picture u -> Picture u -> Picture u
alignVSep align dy p1 p2 = vecMove p1 p2 $ vvec (-dy) ^+^ fn align
  where
    fn VLeft   = bottomleft p1  .-. topleft p2
    fn VCenter = bottommid p1   .-. topmid p2
    fn VRight  = bottomright p1 .-. topright p2



-- | Variant of 'hcat' that aligns the pictures as well as
-- concatenating them.
--
hcatA :: (Fractional u, Ord u) 
      => HAlign -> Picture u -> [Picture u] -> Picture u
hcatA ha = foldl' (alignH ha)

-- | Variant of 'vcat' that aligns the pictures as well as
-- concatenating them.
--
vcatA :: (Fractional u, Ord u) 
      => VAlign -> Picture u -> [Picture u] -> Picture u
vcatA va = foldl' (alignV va)


-- | Variant of @hsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
hsepA :: (Fractional u, Ord u) 
      => HAlign -> u -> Picture u -> [Picture u] -> Picture u
hsepA ha n = foldl' op 
  where 
    a `op` b = alignHSep ha n a b 


-- | Variant of @vsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
vsepA :: (Fractional u, Ord u) 
      => VAlign -> u -> Picture u -> [Picture u] -> Picture u
vsepA va n = foldl' op 
  where 
    a `op` b = alignVSep va n a b 



