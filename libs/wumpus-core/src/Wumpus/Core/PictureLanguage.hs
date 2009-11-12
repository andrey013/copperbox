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
  , PMove(..)
  , PBlank(..)

  -- * Derived operations
  , ( ->- )
  , ( -<- )
  , ( -//- )
  , ( -\\- )
  , at
  , stack
  , hcenter
  , vcenter
  , center
  , ( -@- )
  , stackCenter

  , blankH
  , blankV
  , hspace
  , vspace
 
  ) where

import Wumpus.Core.Geometry ( Point2(..) )

import Data.List ( foldl' )


--------------------------------------------------------------------------------
-- Type family and classes


-- The unit type of /points/ within a Picture.
type family PUnit a


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
class PMove a where
  pmove :: PUnit a -> PUnit a -> a -> a


class PBlank a where
  blank :: PUnit a -> PUnit a -> a



--------------------------------------------------------------------------------
-- Derived operations

infixr 5 -//-
infixr 6 ->-

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
at :: (PMove a, u ~ PUnit a) => Point2 u -> a -> a
at (P2 x y) p = pmove x y p


-- | Stack the pictures using 'over'.
stack :: (PEmpty a, Composite a) => [a] -> a
stack = foldl' over pempty


-- | The (one-dimensional) point midway between the left bound
-- and the right bound.
hcenter :: (Horizontal a, Fractional u, u ~ PUnit a) => a -> u
hcenter a = leftBound a + 0.5 * (rightBound a - leftBound a)


-- | The (one-dimensional) point midway between the left bound
-- and the right bound.
vcenter :: (Vertical a, Fractional u, u ~ PUnit a) => a -> u
vcenter a = bottomBound a + 0.5 * (topBound a - bottomBound a)

center :: (Horizontal a, Vertical a, Fractional u, u ~ PUnit a)
       => a -> Point2 u
center a = P2 (hcenter a) (vcenter a)



(-@-) :: (Horizontal a, Vertical a, Composite a, PMove a, Fractional u, 
             u ~ PUnit a)
         => a -> a -> a
p1 -@- p2 = over p1 (pmove x y p2) where
  x = hcenter p1 - hcenter p2
  y = vcenter p1 - vcenter p2

stackCenter :: (PEmpty a, Horizontal a, Vertical a, Composite a, 
                PMove a, Fractional u,
                u ~ PUnit a)
            => [a] -> a
stackCenter = foldl' (-@-) pempty



--------------------------------------------------------------------------------


blankH  :: (Num u, PBlank a, u ~ PUnit a) => u -> a
blankH = blank `flip` 0

blankV  :: (Num u, PBlank a, u ~ PUnit a) => u -> a
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
hspace :: (Num u, Composite a, Horizontal a, PBlank a, u ~ PUnit a) 
       => u -> a -> a -> a
hspace n a b = a ->- blankH n ->-  b

vspace :: (Num u, Composite a, Vertical a, PBlank a, u ~ PUnit a) 
       => u -> a -> a -> a
vspace n a b = a -//- blankV n -//-  b