{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureLanguage
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.PictureLanguage 
  (

  -- * Type family and classes
    PUnit 
  , PEmpty(..)
  , Horizontal(..)
  , Vertical(..)
  , Composite(..)
  , Move(..)
  , Blank(..)

  -- * Bounds
  , center

  -- * Composition
  , ( ->- )
  , ( -<- )
  , ( -//- )
  , ( -\\- )
  , at
  , stack
  , hcat 
  , vcat
  , ( -@- )
  , stackCenter

  , hspace
  , vspace
  , hsep
  , vsep
 
  ) where

import Wumpus.Core.Geometry ( Point2(..), Vec2(..) )

import Data.AffineSpace

import Data.List ( foldl' )


--------------------------------------------------------------------------------
-- Data types

-- Alignment
{-

data HAlign = HTop | HCenter | HBottom
  deriving (Eq,Show)

data VAlign = VLeft | VCenter | VRight
  deriving (Eq,Show)
-}



--------------------------------------------------------------------------------
-- Type family and classes


-- The unit type of /points/ within a Picture.
type family PUnit a

-- | Create an empty picture.
class PEmpty a where
  pempty :: a


class Horizontal a where
  moveH      :: PUnit a -> a -> a
  leftBound  :: a -> PUnit a
  rightBound :: a -> PUnit a

class Vertical a where
  moveV       :: PUnit a -> a -> a
  topBound    :: a -> PUnit a
  bottomBound :: a -> PUnit a

class Composite a where
  over    :: a -> a -> a
  beneath :: a -> a -> a

  beneath = flip over
  
-- Move in 2D
class Move a where
  move :: PUnit a -> PUnit a -> a -> a


class Blank a where
  blank :: PUnit a -> PUnit a -> a



--------------------------------------------------------------------------------

-- Operations on bounds

-- | The center of a picture.
center :: (Horizontal a, Vertical a, Fractional u, u ~ PUnit a)
       => a -> Point2 u
center a = P2 hcenter vcenter where  
    hcenter = leftBound a + 0.5 * (rightBound a - leftBound a)
    vcenter = bottomBound a + 0.5 * (topBound a - bottomBound a)



--------------------------------------------------------------------------------
-- Composition

infixr 5 -//-
infixr 6 ->-, -@-


-- | Center the pic1 on top of pic2.
(-@-) :: (Horizontal a, Vertical a, Composite a, Move a, Fractional u, 
             u ~ PUnit a)
         => a -> a -> a
p1 -@- p2 = p1 `over` (move x y p2) where V2 x y = center p1 .-. center p2


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


-- | Stack the pictures using 'over' - the first picture in the 
-- list is drawn at the top, last picture is on drawn at the 
-- bottom.
stack :: (PEmpty a, Composite a) => [a] -> a
stack = foldl' over pempty

hcat :: (PEmpty a, Horizontal a, Composite a, Num u, u ~ PUnit a)
     => [a] -> a
hcat = foldl' (->-) pempty 

vcat :: (PEmpty a, Vertical a, Composite a, Num u, u ~ PUnit a)
     => [a] -> a
vcat = foldl' (-//-) pempty



-- | Stack pictures centered ontop of each other - the first 
-- picture in the list is drawn at the top, last picture is on 
-- drawn at the bottom.
stackCenter :: (PEmpty a, Horizontal a, Vertical a, Composite a, 
                Move a, Fractional u,
                u ~ PUnit a)
            => [a] -> a
stackCenter = foldl' (-@-) pempty



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

hsep :: (Num u, PEmpty a, Composite a, Horizontal a, Blank a, u ~ PUnit a) 
       => u -> [a] -> a
hsep _ []     = pempty
hsep n (x:xs) = foldl' (hspace n) x xs 

vsep :: (Num u, PEmpty a, Composite a, Vertical a, Blank a, u ~ PUnit a) 
       => u -> [a] -> a
vsep _ []     = pempty
vsep n (x:xs) = foldl' (vspace n) x xs 


--------------------------------------------------------------------------------
-- Aligning pictures

{-

-- TODO Next

alignH :: HAlign -> a -> a -> a
alignH ha a b = undefined

-}