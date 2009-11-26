{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureLanguage
-- Copyright   :  (c) Stephen Tetley 2009
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
  , ( -\\- )
  , at
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

import Wumpus.Core.Geometry ( Point2(..), Vec2(..) )

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

infixr 5 -//-
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


-- | Horizontal composition - place @b@ at the right of @a@.
(->-) :: (Horizontal a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a ->- b = over a (moveH disp b) where disp = rightBound a - leftBound b 

-- | Horizontal composition - place @a@ at the left of @b@.
(-<-) :: (Horizontal a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
(-<-) = flip (->-)   -- TO TEST...

-- | Vertical composition - place @b@ below @a@.
(-//-) :: (Vertical a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
a -//- b = over a (moveV disp b) where disp = bottomBound a - topBound b 

-- | Vertical composition - place @a@ above @b@.
(-\\-) :: (Vertical a, Composite a, Num u, u ~ PUnit a) => a -> a -> a
(-\\-) = flip (-//-)


-- | Place the picture at the supplied point.
at :: (Move a, u ~ PUnit a) => a -> Point2 u  -> a
p `at` (P2 x y) = move x y p


-- stackOnto :: [a] -> a -> a
-- This would obviate the need for pempty without needing a 
-- non-empty list

-- | Stack the pictures using 'over' - the first picture in the 
-- list is drawn at the top, last picture is on drawn at the 
-- bottom.
stackOnto :: (Composite a) => [a] -> a -> a
stackOnto = flip (foldr over)

hcat :: (Horizontal a, Composite a, Num u, u ~ PUnit a)
     => a -> [a] -> a
hcat = foldl' (->-)

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


blankH  :: (Num u, Blank a, u ~ PUnit a) => u -> a
blankH = blank `flip` 0

blankV  :: (Num u, Blank a, u ~ PUnit a) => u -> a
blankV = blank 0



-- | The following simple definition of hspace is invalid:
--
-- > hspace n a b = a ->- (moveH n b)
-- 
-- The movement due to @moveH n@ is annulled by the @->-@ 
-- operator which moves relative to the bounding box.
-- 
-- The almost as simple definition below, seems to justify 
-- including Blank as a Picture constructor.
--
hspace :: (Num u, Composite a, Horizontal a, Blank a, u ~ PUnit a) 
       => u -> a -> a -> a
hspace n a b = a ->- blankH n ->-  b

vspace :: (Num u, Composite a, Vertical a, Blank a, u ~ PUnit a) 
       => u -> a -> a -> a
vspace n a b = a -//- blankV n -//-  b

hsep :: (Num u, Composite a, Horizontal a, Blank a, u ~ PUnit a) 
       => u -> a -> [a] -> a
hsep n = foldl' (hspace n)

vsep :: (Num u, Composite a, Vertical a, Blank a, u ~ PUnit a) 
       => u -> a -> [a] -> a
vsep n = foldl' (vspace n)


--------------------------------------------------------------------------------
-- Aligning pictures


vecMove :: (Composite a, Move a, u ~ PUnit a) => a -> a -> (Vec2 u) -> a 
vecMove a b (V2 x y) = a `over` (move x y) b 

alignH :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => HAlign -> a -> a -> a
alignH HTop    p1 p2 = vecMove p1 p2 (topright p1    .-. topleft p2)
alignH HCenter p1 p2 = vecMove p1 p2 (rightmid p1    .-. leftmid p2)
alignH HBottom p1 p2 = vecMove p1 p2 (bottomright p1 .-. bottomleft p2)

alignV :: ( Fractional u, Composite a, Horizontal a, Vertical a, Move a
          , u ~ PUnit a ) 
       => VAlign -> a -> a -> a
alignV VLeft   p1 p2 = vecMove p1 p2 (bottomleft p1  .-. topleft p2)
alignV VCenter p1 p2 = vecMove p1 p2 (bottommid p1   .-. topmid p2)
alignV VRight  p1 p2 = vecMove p1 p2 (bottomright p1 .-. topright p2)


hcatA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, u ~ PUnit a)
     => HAlign -> a -> [a] -> a
hcatA ha = foldl' (alignH ha)

vcatA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, u ~ PUnit a)
     => VAlign -> a -> [a] -> a
vcatA va = foldl' (alignV va)



hsepA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, Blank a, u ~ PUnit a)
     => HAlign -> u -> a -> [a] -> a
hsepA ha n = foldl' op where 
   a `op` b = alignH ha (alignH ha a (blankH n)) b 

vsepA :: ( Fractional u, Horizontal a, Vertical a
         , Composite a, Move a, Blank a, u ~ PUnit a)
     => VAlign -> u -> a -> [a] -> a
vsepA va n = foldl' op where 
   a `op` b = alignV va (alignV va a (blankV n)) b 

