{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.DrawingComposition
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Composition operators for Drawings.
--
-- Note - some operations can produce empty drawings...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.DrawingComposition
  (
  -- * Data types for alignment 
    HAlign(..)
  , VAlign(..)


  -- * Composition
  , over 
  , under

  , centerOver
  , nextToH
  , nextToV
  
  , atPoint 
  , centeredAt

  , zconcat

  , hcat 
  , vcat


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

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.Drawing

import Wumpus.Core                              -- package: wumpus-core

import Data.AdditiveGroup
import Data.AffineSpace

import Control.Applicative
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

  
-- Note - do not export the empty drawing. It is easier to 
-- pretend it doesn't exist.
-- 
empty_drawing :: (Real u, Floating u, FromPtSize u) => Drawing u
empty_drawing = drawTracing $ return ()


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

-- Note - these operators are in some ways /anti-combinators/, it
-- seems easier to think about composing drawings if we do 
-- work on the result Pictures directly rather than build 
-- combinators to manipulate Drawings (I hope this works...)
--



megaCombL :: (Picture u -> a) -> (Picture u -> a) 
          -> (a -> a -> Picture u -> Picture u) 
          -> (Picture u -> Picture u -> Picture u)
          -> Drawing u -> Drawing u
          -> Drawing u
megaCombL qL qR trafoL comb = drawingConcat fn
  where
    fn pic1 pic2 = let a    = qL pic1
                       b    = qR pic2
                       p1   = trafoL a b pic1
                   in p1 `comb` pic2


megaCombR :: (Picture u -> a) -> (Picture u -> a) 
          -> (a -> a -> Picture u -> Picture u) 
          -> (Picture u -> Picture u -> Picture u)
          -> Drawing u -> Drawing u
          -> Drawing u
megaCombR qL qR trafoR comb = drawingConcat fn
  where
    fn pic1 pic2 = let a    = qL pic1
                       b    = qR pic2
                       p2   = trafoR a b pic2
                   in pic1 `comb` p2




-- | > a `over` b
-- 
-- Place \'drawing\' a over b. The idea of @over@ here is in 
-- terms z-ordering, nither picture a or b are actually moved.
--
over    :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
over    = drawingConcat picOver



-- | > a `under` b
--
-- Similarly @under@ draws the first drawing behind 
-- the second but move neither.
--
under :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
under = flip over



-- | Move in both the horizontal and vertical.
--
move :: (Num u, Ord u) => Vec2 u -> Drawing u -> Drawing u
move v = modifyDrawing (\p -> p `picMoveBy` v)







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

-- simpler to use BB directly...

leftmid       :: Fractional u => Picture u -> Point2 u
leftmid       = fn . boundary
   where
     fn (BBox (P2 llx lly) (P2 _ ury)) = P2 llx (midpt lly ury)



rightmid      :: Fractional u => Picture u -> Point2 u
rightmid      = fn . boundary
  where
    fn (BBox (P2 _ lly) (P2 urx ury)) = P2 urx (midpt lly ury)


topmid        :: Fractional u => Picture u -> Point2 u
topmid        = fn . boundary
  where
    fn (BBox (P2 llx _) (P2 urx ury)) = P2 (midpt llx urx) ury


bottommid     :: Fractional u => Picture u -> Point2 u
bottommid     = fn . boundary
  where
    fn (BBox (P2 llx lly) (P2 urx _)) = P2 (midpt llx urx) lly

midpt :: Fractional a => a -> a -> a
midpt a b = a + 0.5*(b-a)


--------------------------------------------------------------------------------
-- Composition

-- infixr 5 `nextToV`
-- infixr 6 `nextToH`, `centerOver`




-- | Draw a centered over b - a is moved, b is static.
--
-- > a `centerOver` b 
--
-- Note - `centerOver` moves the first argument, whereas most 
-- other functions move the second
-- 
centerOver :: (Fractional u, Ord u) => Drawing u -> Drawing u -> Drawing u
centerOver = megaCombL centerPoint centerPoint moveFun picOver
  where
    moveFun p1 p2 pic =  let v = p2 .-. p1 in pic `picMoveBy` v



-- | > a `nextToH` b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
nextToH :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
nextToH = megaCombR rightBound leftBound moveFun picOver 
  where 
    moveFun a b pic = pic `picMoveBy` hvec (a - b)



-- | > a `nextToV` b
--
-- Vertical composition - move @b@, placing it below @a@.
--
nextToV :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
nextToV = megaCombR bottomBound topBound moveFun picOver 
  where 
    moveFun a b drw = drw `picMoveBy` vvec (a - b)


-- | Place the picture at the supplied point.
--
-- `atPoint` was previous the `at` operator.
-- 
atPoint :: (Num u, Ord u) => Drawing u -> Point2 u  -> Drawing u
p `atPoint` (P2 x y) = move (V2 x y) p



-- | Center the picture at the supplied point.
--
centeredAt :: (Fractional u, Ord u) => Drawing u -> Point2 u -> Drawing u
centeredAt d (P2 x y) = modifyDrawing fn d
  where
    fn p = let bb = boundary p
               dx = x - (boundaryWidth  bb * 0.5)
               dy = y - (boundaryHeight bb * 0.5)
           in p `picMoveBy` vec dx dy


-- | Concatenate the list of drawings. 
--
-- No pictures are moved. 
--
zconcat :: (Real u, Floating u, FromPtSize u) => [Drawing u] -> Drawing u
zconcat []     = empty_drawing
zconcat (d:ds) = foldl' over d ds




-- | Concatenate the list pictures @xs@ horizontally.
-- 
hcat :: (Real u, Floating u, FromPtSize u) => [Drawing u] -> Drawing u
hcat []     = empty_drawing
hcat (d:ds) = foldl' nextToH d ds


-- | Concatenate the list of pictures @xs@ vertically.
--
vcat :: (Real u, Floating u, FromPtSize u) => [Drawing u] -> Drawing u
vcat []     = empty_drawing
vcat (d:ds) = foldl' nextToV d ds




--------------------------------------------------------------------------------




-- | > hspace n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
hspace :: (Num u, Ord u) => u -> Drawing u -> Drawing u -> Drawing u
hspace n = megaCombR rightBound leftBound moveFun picOver
  where
    moveFun a b pic = pic `picMoveBy` hvec (n + a - b)

    



-- | > vspace n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
vspace :: (Num u, Ord u) => u -> Drawing u -> Drawing u -> Drawing u
vspace n = megaCombR bottomBound topBound moveFun picOver 
  where 
    moveFun a b pic = pic `picMoveBy`  vvec (a - b - n)



-- | > hsep n xs
--
-- Concatenate the list of pictures @xs@ horizontally with 
-- @hspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
hsep :: (Real u, Floating u, FromPtSize u) => u -> [Drawing u] -> Drawing u
hsep _ []     = empty_drawing
hsep n (d:ds) = foldl' (hspace n) d ds



-- | > vsep n xs
--
-- Concatenate the list of pictures @xs@ vertically with 
-- @vspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
vsep :: (Real u, Floating u, FromPtSize u) => u -> [Drawing u] -> Drawing u
vsep _ []     = empty_drawing
vsep n (d:ds) = foldl' (vspace n) d ds


--------------------------------------------------------------------------------
-- Aligning pictures

alignMove :: (Num u, Ord u) => Point2 u -> Point2 u -> Picture u -> Picture u
alignMove p1 p2 pic = pic `picMoveBy` (p1 .-. p2)



-- | > alignH align a b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ and align it with the top, center or bottom of @a@.
-- 
alignH :: (Fractional u, Ord u) 
       =>  HAlign -> Drawing u -> Drawing u -> Drawing u
alignH HTop     = megaCombR topright    topleft     alignMove  picOver
alignH HCenter  = megaCombR rightmid    leftmid     alignMove  picOver
alignH HBottom  = megaCombR bottomright bottomleft  alignMove  picOver


-- | > alignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
alignV :: (Fractional u, Ord u) 
       => VAlign -> Drawing u -> Drawing u -> Drawing u
alignV VLeft    = megaCombR bottomleft  topleft   alignMove  picOver
alignV VCenter  = megaCombR bottommid   topmid    alignMove  picOver
alignV VRight   = megaCombR bottomright topright  alignMove  picOver



alignMove2 :: (Num u, Ord u) 
           => Vec2 u ->  Point2 u -> Point2 u -> Picture u -> Picture u
alignMove2 v p1 p2 pic = pic `picMoveBy` (v ^+^ (p1 .-. p2))



-- | > alignHSep align sep a b
-- 
-- Spacing version of alignH - move @b@ to the right of @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignHSep :: (Fractional u, Ord u) 
          => HAlign -> u -> Drawing u -> Drawing u -> Drawing u
alignHSep HTop    dx = megaCombR topright topleft (alignMove2 (hvec dx)) picOver
alignHSep HCenter dx = megaCombR rightmid leftmid (alignMove2 (hvec dx)) picOver
alignHSep HBottom dx = megaCombR bottomright bottomleft (alignMove2 (hvec dx)) picOver


-- | > alignVSep align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignVSep :: (Fractional u, Ord u) 
          => VAlign -> u -> Drawing u -> Drawing u -> Drawing u
alignVSep VLeft   dy = megaCombR bottomleft topleft (alignMove2 $ vvec (-dy)) picOver
alignVSep VCenter dy = megaCombR bottommid  topmid  (alignMove2 $ vvec (-dy)) picOver
alignVSep VRight  dy = megaCombR bottomright topright (alignMove2 $ vvec (-dy)) picOver


-- | Variant of 'hcat' that aligns the pictures as well as
-- concatenating them.
--
hcatA :: (Real u, Floating u, FromPtSize u) 
      => HAlign -> [Drawing u] -> Drawing u
hcatA _  []     = empty_drawing
hcatA ha (d:ds) = foldl' (alignH ha) d ds



-- | Variant of 'vcat' that aligns the pictures as well as
-- concatenating them.
--
vcatA :: (Real u, Floating u, FromPtSize u) 
      => VAlign -> [Drawing u] -> Drawing u
vcatA _  []     = empty_drawing
vcatA va (d:ds) = foldl' (alignV va) d ds


-- | Variant of @hsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
hsepA :: (Real u, Floating u, FromPtSize u) 
      => HAlign -> u -> [Drawing u] -> Drawing u
hsepA _  _ []     = empty_drawing
hsepA ha n (d:ds) = foldl' op d ds
  where 
    a `op` b = alignHSep ha n a b 


-- | Variant of @vsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
vsepA :: (Real u, Floating u, FromPtSize u) 
      => VAlign -> u -> [Drawing u] -> Drawing u
vsepA _  _ []     = empty_drawing
vsepA va n (d:ds) = foldl' op d ds
  where 
    a `op` b = alignVSep va n a b 



