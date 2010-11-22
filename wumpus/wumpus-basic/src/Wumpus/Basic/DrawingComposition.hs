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
  -- * Composition
    over 
  , under

  , centric
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

import Wumpus.Core                              -- package: wumpus-core

import Data.AdditiveGroup
import Data.AffineSpace

import Data.List ( foldl' )



--------------------------------------------------------------------------------
-- Extract anchors

boundaryExtr :: (BoundingBox u -> a) -> Picture u -> a
boundaryExtr f = f . boundary

-- Operations on bounds

-- | The center of a picture.
--
boundaryCenter :: Fractional u => Picture u -> Point2 u
boundaryCenter = boundaryExtr center



-- | Extract the mid point of the top edge.
--
boundaryN :: Fractional u => Picture u -> Point2 u
boundaryN = boundaryExtr north

-- | Extract the mid point of the bottom edge.
--
boundaryS :: Fractional u => Picture u -> Point2 u
boundaryS = boundaryExtr south

-- | Extract the mid point of the left edge.
--
boundaryE :: Fractional u => Picture u -> Point2 u
boundaryE = boundaryExtr east

-- | Extract the mid point of the right edge.
--
boundaryW :: Fractional u => Picture u -> Point2 u
boundaryW = boundaryExtr west


-- | Extract the top-left corner.
--
boundaryNW :: Fractional u => Picture u -> Point2 u
boundaryNW = boundaryExtr northwest

-- | Extract the top-right corner.
--
boundaryNE :: Picture u -> Point2 u
boundaryNE = boundaryExtr ur_corner

-- | Extract the bottom-left corner.
--
boundarySW :: Picture u -> Point2 u
boundarySW = boundaryExtr ll_corner

-- | Extract the bottom-right corner.
--
boundarySE :: Fractional u => Picture u -> Point2 u
boundarySE = boundaryExtr southeast


boundaryLeftEdge :: Picture u -> u
boundaryLeftEdge = boundaryExtr (point_x . ll_corner)

boundaryRightEdge :: Picture u -> u
boundaryRightEdge = boundaryExtr (point_x . ur_corner)

boundaryBottomEdge :: Picture u -> u
boundaryBottomEdge = boundaryExtr (point_y . ll_corner)


boundaryTopEdge :: Picture u -> u
boundaryTopEdge = boundaryExtr (point_y . ur_corner)




  
-- Note - do not export the empty drawing. It is easier to 
-- pretend it doesn't exist.
-- 
empty_drawing :: (Real u, Floating u, FromPtSize u) => Drawing u
empty_drawing = drawTracing $ return ()



--------------------------------------------------------------------------------
-- Composition operators

-- Note - the megaCombR operator is in some way an
-- /anti-combinator/. It seems easier to think about composing 
-- drawings if we do work on the result Pictures directly rather 
-- than build combinators to manipulate Drawings.
--
-- The idea of combining pre- and post- operating combinators
-- makes me worry about circular programs even though I know 
-- lazy evaluation allows me to write them (in some cicumstances).
--


-- Picture /mega-combiner/ - moves only the second argument aka the 
-- right picture.
--
megaCombR :: (Num u, Ord u)
          => (Picture u -> a) -> (Picture u -> a) 
          -> (a -> a -> Picture u -> Picture u) 
          -> Drawing u -> Drawing u
          -> Drawing u
megaCombR qL qR trafoR = drawingConcat fn
  where
    fn pic1 pic2 = let a    = qL pic1
                       b    = qR pic2
                       p2   = trafoR a b pic2
                   in pic1 `picOver` p2




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




--------------------------------------------------------------------------------
-- Composition

infixr 5 `nextToV`
infixr 6 `nextToH`, `centric`




-- | Draw @a@, move @b@ so its center is at the same center as 
-- @a@, @b@ is drawn over underneath in the zorder.
--
-- > a `centeric` b 
--
--
centric :: (Fractional u, Ord u) => Drawing u -> Drawing u -> Drawing u
centric = megaCombR boundaryCenter boundaryCenter moveFun
  where
    moveFun p1 p2 pic =  let v = p1 .-. p2 in pic `picMoveBy` v



-- | > a `nextToH` b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
nextToH :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
nextToH = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where 
    moveFun a b pic = pic `picMoveBy` hvec (a - b)



-- | > a `nextToV` b
--
-- Vertical composition - move @b@, placing it below @a@.
--
nextToV :: (Num u, Ord u) => Drawing u -> Drawing u -> Drawing u
nextToV = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
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
hspace n = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where
    moveFun a b pic = pic `picMoveBy` hvec (n + a - b)

    



-- | > vspace n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
vspace :: (Num u, Ord u) => u -> Drawing u -> Drawing u -> Drawing u
vspace n = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
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
alignH HTop     = megaCombR boundaryNE    boundaryNW     alignMove
alignH HCenter  = megaCombR boundaryE    boundaryW     alignMove
alignH HBottom  = megaCombR boundarySE boundarySW  alignMove


-- | > alignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
alignV :: (Fractional u, Ord u) 
       => VAlign -> Drawing u -> Drawing u -> Drawing u
alignV VLeft    = megaCombR boundarySW  boundaryNW   alignMove
alignV VCenter  = megaCombR boundaryS   boundaryN    alignMove
alignV VRight   = megaCombR boundarySE boundaryNE  alignMove



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
alignHSep HTop    dx = megaCombR boundaryNE boundaryNW (alignMove2 (hvec dx))
alignHSep HCenter dx = megaCombR boundaryE  boundaryW  (alignMove2 (hvec dx))
alignHSep HBottom dx = megaCombR boundarySE boundarySW (alignMove2 (hvec dx))


-- | > alignVSep align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignVSep :: (Fractional u, Ord u) 
          => VAlign -> u -> Drawing u -> Drawing u -> Drawing u
alignVSep VLeft   dy = megaCombR boundarySW boundaryNW (alignMove2 $ vvec (-dy)) 
alignVSep VCenter dy = megaCombR boundaryS  boundaryN  (alignMove2 $ vvec (-dy)) 
alignVSep VRight  dy = megaCombR boundarySE boundaryNE (alignMove2 $ vvec (-dy))


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



