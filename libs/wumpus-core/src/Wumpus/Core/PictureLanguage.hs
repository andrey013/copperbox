{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureLanguage
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Type classes and derived functions to compose 2D /pictures/.
--
-- The operations are fairly standard - see Regions in Paul 
-- Hudak\'s \'The Haskell School of Expression' and the pretty
-- printing libraries wl-pprint and Text.PrettyPrint.HughesPJ 
-- (pretty printing combinators are some ways in \'One and a 
-- half D\' as they have horizontal operations but only carriage 
-- return in the vertical.
--
--------------------------------------------------------------------------------

module Wumpus.Core.PictureLanguage 
  (
  -- * Data types for alignment 
    HAlign(..)
  , VAlign(..)

  -- * Type family and classes
  , PUnit 
  , Horizontal(..)
  , Vertical(..)
  , Composite(..)
  , Move(..)
  , Blank(..)

  -- * Bounds
  -- $boundsdoc
  , center
  , topleft
  , topright
  , bottomleft
  , bottomright

  -- * Composition
  , ( -@- )
  , ( ->- )
  , ( -<- )
  , ( -//- )
  , above
  , below
  , at
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

import Wumpus.Core.Geometry

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
-- Type family and classes


-- | The type of /points/ within a Picture.
type family PUnit a


-- | > a `over` b
-- 
-- Place \'picture\' a over b. The idea of @over@ here is the same
-- as z-ordering in 2D design programs. Implementations of this 
-- class should \'draw\' picture a over b but move neither.
-- 
-- Similarly @beneath@ should \'draw\' the first picture behind 
-- the second but move neither.
--
-- Beneath has a default definition:
--
-- > beneath = flip over
--
class Composite a where
  over    :: a -> a -> a
  beneath :: a -> a -> a

  beneath = flip over


-- | Create a /picture/ that has no content but occupies space 
-- (i.e. it has a bounding box).
class Blank a where
  blank :: PUnit a -> PUnit a -> a


-- | Move horizontally.
class Horizontal a where
  moveH      :: PUnit a -> a -> a
  leftBound  :: a -> PUnit a
  rightBound :: a -> PUnit a

-- | Move vertically.
class Vertical a where
  moveV       :: PUnit a -> a -> a
  topBound    :: a -> PUnit a
  bottomBound :: a -> PUnit a


  
-- | Move in both the horizontal and vertical.
class Move a where
  move :: PUnit a -> PUnit a -> a -> a




--------------------------------------------------------------------------------

-- Operations on bounds

-- $boundsdoc
-- Corresponding operations are available on bounding boxes - the 
-- definitions here have different type class obligations.

-- | The center of a picture.
center :: (Horizontal a, Vertical a, Fractional u, u ~ PUnit a) => a -> Point2 u
center a = P2 hcenter vcenter where  
    hcenter = leftBound a   + 0.5 * (rightBound a - leftBound a)
    vcenter = bottomBound a + 0.5 * (topBound a   - bottomBound a)

-- | Extract the top-left corner.
topleft       :: (Horizontal a, Vertical a, u ~ PUnit a) => a -> Point2 u
topleft a     = P2 (leftBound a)  (topBound a)

-- | Extract the top-right corner.
topright      :: (Horizontal a, Vertical a, u ~ PUnit a) => a -> Point2 u
topright a    = P2 (rightBound a) (topBound a)

-- | Extract the bottom-left corner.
bottomleft    :: (Horizontal a, Vertical a, u ~ PUnit a) => a -> Point2 u
bottomleft a  = P2 (leftBound a)  (bottomBound a)

-- | Extract the bottom-right corner.
bottomright   :: (Horizontal a, Vertical a, u ~ PUnit a) => a -> Point2 u
bottomright a = P2 (rightBound a) (bottomBound a)

--------------------------------------------------------------------------------
-- Internal helpers

leftmid       :: (Fractional u, Horizontal a, Vertical a, u ~ PUnit a) 
              => a -> Point2 u
leftmid a     = P2 (leftBound a) (midpt (bottomBound a) (topBound a))

rightmid      :: (Fractional u, Horizontal a, Vertical a, u ~ PUnit a) 
              => a -> Point2 u
rightmid a    = P2 (rightBound a) (midpt (bottomBound a) (topBound a))


topmid        :: (Fractional u, Horizontal a, Vertical a, u ~ PUnit a) 
              => a -> Point2 u
topmid a      = P2 (midpt (leftBound a) (rightBound a)) (topBound a)

bottommid     :: (Fractional u, Horizontal a, Vertical a, u ~ PUnit a) 
              => a -> Point2 u
bottommid a   = P2 (midpt (leftBound a) (rightBound a)) (bottomBound a)


midpt :: Fractional a => a -> a -> a
midpt a b = a + 0.5*(b-a)

--------------------------------------------------------------------------------
-- Composition

infixr 5 -//-, `above`, `below`
infixr 6 ->-, -@-


-- | > a -@- b
-- 
-- Center @a@ on top of @b@, @a@ is potentially moved and drawn 
-- 'over' @b@.
--
(-@-) :: (Horizontal a, Vertical a, Composite a, Move a, Fractional u, 
             u ~ PUnit a)
         => a -> a -> a
p1 -@- p2 = (move x y p1) `over` p2 where V2 x y = center p2 .-. center p1


-- | > a ->- b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
(->-) :: (Horizontal a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a ->- b = a `over` (moveH disp b) where disp = rightBound a - leftBound b 

-- | > a -<- b
-- 
-- Horizontal composition - move @a@, placing it to the left 
-- of @b@.
--
(-<-) :: (Horizontal a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a -<- b = (moveH disp a) `over` b where disp = leftBound b - rightBound a


-- | > a -//- b
--
-- Vertical composition - move @b@, placing it below @a@.
--
(-//-) :: (Vertical a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a -//- b = a `over` (moveV disp b) where disp = bottomBound a - topBound b 


-- | > a `below` b
-- 
-- Vertical composition - move @a@, placing it below @b@
--
below :: (Vertical a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a `below` b = (moveV disp a) `over` b where disp = bottomBound a - topBound b



-- | > a `above` b
-- 
-- Vertical composition - move @a@, placing it above @b@.
--
above :: (Vertical a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a `above` b = (moveV disp a) `over` b where disp = topBound b - bottomBound a 


-- | Place the picture at the supplied point.
-- 
at :: (Move a, u ~ PUnit a) => a -> Point2 u  -> a
p `at` (P2 x y) = move x y p

-- | Center the picture at the supplied point.
--
centeredAt :: (Horizontal a, Vertical a, Move a, Composite a, Blank a, 
               Fractional u, u ~ PUnit a) 
           => a -> Point2 u -> a
centeredAt p pt = p -@- (blank 0 0 `at` pt) 



-- | > xs `stackOnto` a
-- 
-- Stack the list of pictures @xs@ 'over' @a@.
--
-- Note, the first picture in the list is drawn at the top, the
-- last picture is draw 'over' @a@.
--
stackOnto :: (Composite a) => [a] -> a -> a
stackOnto = flip (foldr over)

-- | > x ->- xs
-- 
-- Concatenate the list pictures @xs@ horizontally with @(->-)@ 
-- starting at @x@.
-- 
hcat :: (Horizontal a, Composite a, Num u, u ~ PUnit a)
     => a -> [a] -> a
hcat = foldl' (->-)

-- | > x -//- xs
-- 
-- Concatenate the list of pictures @xs@ vertically with @(-\/\/-)@ 
-- starting at @x@.
--
vcat :: (Vertical a, Composite a, Num u, u ~ PUnit a)
     => a -> [a] -> a
vcat = foldl' (-//-)



-- | Stack pictures centered ontop of each other - the first 
-- picture in the list is drawn at the top, last picture is on 
-- drawn at the bottom.
stackOntoCenter :: (Horizontal a, Vertical a, Composite a, 
                Move a, Fractional u,
                u ~ PUnit a)
            => [a] -> a -> a
stackOntoCenter = flip $ foldr (-@-)



--------------------------------------------------------------------------------

-- Helpers
blankH  :: (Num u, Blank a, u ~ PUnit a) => u -> a
blankH = blank `flip` 0

blankV  :: (Num u, Blank a, u ~ PUnit a) => u -> a
blankV = blank 0


-- NOTE
-- The following simple definition of hspace is invalid:
--
-- > hspace n a b = a ->- (moveH n b)
-- 
-- The movement due to @moveH n@ is annulled by the @->-@ 
-- operator which moves relative to the bounding box.
-- 
-- The almost as simple definition below, seems to justify 
-- including Blank as a Picture constructor.
--


-- | > hspace n a b
--
-- Concatenate the pictures @a@ and @b@ with @(->-)@ - injecting 
-- a space of @n@ units to separate the pictures.
--
hspace :: (Num u, Composite a, Horizontal a, Blank a, u ~ PUnit a) 
       => u -> a -> a -> a
hspace n a b = a ->- blankH n ->- b

-- | > vspace n a b
--
-- Concatenate the pictures @a@ and @b@ with @(-\/\/-)@ - injecting 
-- a space of @n@ units to separate the pictures.
--
vspace :: (Num u, Composite a, Vertical a, Blank a, u ~ PUnit a) 
       => u -> a -> a -> a
vspace n a b = a -//- blankV n -//-  b



-- | > hsep n x xs
--
-- Concatenate the list of pictures @xs@ horizontally with 
-- @hspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
hsep :: (Num u, Composite a, Horizontal a, Blank a, u ~ PUnit a) 
       => u -> a -> [a] -> a
hsep n = foldl' (hspace n)



-- | > vsep n x xs
--
-- Concatenate the list of pictures @xs@ vertically with 
-- @vspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
vsep :: (Num u, Composite a, Vertical a, Blank a, u ~ PUnit a) 
       => u -> a -> [a] -> a
vsep n = foldl' (vspace n)


--------------------------------------------------------------------------------
-- Aligning pictures


-- | > alignH z a b
--
-- Move picture @b@ up or down to be horizontally aligned along a 
-- line from the top, center or bottom of picture @a@
-- 
alignH :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => HAlign -> a -> a -> a
alignH HTop    p1 p2 = vecMove p1 p2 (vvec $ topBound p1 - topBound p2)
alignH HBottom p1 p2 = vecMove p1 p2 (vvec $ bottomBound p1 - bottomBound p2)
alignH HCenter p1 p2 = vecMove p1 p2 (vvec v)
  where V2 _ v = rightmid p1    .-. leftmid p2


-- | > alignV z a b
--
-- Move picture @b@ left or right to be vertically aligned along a 
-- line from the left side, center or right side of picture @a@
-- 
alignV :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => VAlign -> a -> a -> a
alignV VLeft   p1 p2 = vecMove p1 p2 (hvec $ leftBound p1 - leftBound p2) 
alignV VRight  p1 p2 = vecMove p1 p2 (hvec $ rightBound p1 - rightBound p2)
alignV VCenter p1 p2 = vecMove p1 p2 (hvec h) 
  where V2 h _ = bottommid p1   .-. topmid p2


-- Helpers

vecMove :: (Composite a, Move a, u ~ PUnit a) => a -> a -> (Vec2 u) -> a 
vecMove a b (V2 x y) = a `over` (move x y) b 

-- Unlike alignH this function \"moves and concatenates\".
moveAlignH :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => HAlign -> a -> a -> a
moveAlignH HTop    p1 p2 = vecMove p1 p2 (topright p1    .-. topleft p2)
moveAlignH HCenter p1 p2 = vecMove p1 p2 (rightmid p1    .-. leftmid p2)
moveAlignH HBottom p1 p2 = vecMove p1 p2 (bottomright p1 .-. bottomleft p2)


-- Unlike alignV this function \"moves and concatenates\".
moveAlignV :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => VAlign -> a -> a -> a
moveAlignV VLeft   p1 p2 = vecMove p1 p2 (bottomleft p1  .-. topleft p2)
moveAlignV VCenter p1 p2 = vecMove p1 p2 (bottommid p1   .-. topmid p2)
moveAlignV VRight  p1 p2 = vecMove p1 p2 (bottomright p1 .-. topright p2)


-- | Variant of 'hcat' that aligns the pictures as well as
-- concatenating them.
hcatA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, u ~ PUnit a)
     => HAlign -> a -> [a] -> a
hcatA ha = foldl' (moveAlignH ha)

-- | Variant of 'vcat' that aligns the pictures as well as
-- concatenating them.
vcatA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, u ~ PUnit a)
     => VAlign -> a -> [a] -> a
vcatA va = foldl' (moveAlignV va)


-- | Variant of @hsep@ that aligns the pictures as well as
-- concatenating and spacing them.
hsepA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, Blank a, u ~ PUnit a)
     => HAlign -> u -> a -> [a] -> a
hsepA ha n = foldl' op where 
   a `op` b = moveAlignH ha (moveAlignH ha a (blankH n)) b 

-- | Variant of @vsep@ that aligns the pictures as well as
-- concatenating and spacing them.
vsepA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, Blank a, u ~ PUnit a)
     => VAlign -> u -> a -> [a] -> a
vsepA va n = foldl' op where 
   a `op` b = moveAlignV va (moveAlignV va a (blankV n)) b 

