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
  -- * Type classes
    PEmpty(..)
  , Horizontal(..)
  , Vertical(..)
  , Composite(..)
  , PMove(..)

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
 
  ) where

import Wumpus.Core.Geometry ( Point2(..) )

import Data.List ( foldl' )


--------------------------------------------------------------------------------
-- Type classes

class PEmpty a where
  pempty :: a

class Horizontal a where
  type HUnit a :: *
  moveH      :: HUnit a -> a -> a
  leftBound  :: a -> HUnit a
  rightBound :: a -> HUnit a

class Vertical a where
  type VUnit a :: *
  moveV       :: VUnit a -> a -> a
  topBound    :: a -> VUnit a
  bottomBound :: a -> VUnit a

class Composite a where
  over    :: a -> a -> a
  beneath :: a -> a -> a

  beneath = flip over
  
-- Move in 2D
class (Vertical a, Horizontal a) => PMove a where
  pmove   :: HUnit a -> VUnit a -> a -> a


--------------------------------------------------------------------------------
-- Derived operations

infixr 5 -//-
infixr 6 ->-

-- | Horizontal composition - place @b@ at the right of @a@.
(->-) :: (Horizontal a, Composite a, Num u, u ~ HUnit a) => a -> a -> a
a ->- b = over a (moveH disp b) where disp = rightBound a - leftBound b 

-- | Horizontal composition - place @a@ at the left of @b@.
(-<-) :: (Horizontal a, Composite a, Num u, u ~ HUnit a) => a -> a -> a
(-<-) = flip (->-)   -- TO TEST...

-- | Vertical composition - place @b@ below @a@.
(-//-) :: (Vertical a, Composite a, Num u, u ~ VUnit a) => a -> a -> a
a -//- b = over a (moveV disp b) where disp = bottomBound a - topBound b 

-- | Vertical composition - place @a@ above @b@.
(-\\-) :: (Vertical a, Composite a, Num u, u ~ VUnit a) => a -> a -> a
(-\\-) = flip (-//-)


-- | Place the picture at the supplied point.
at :: (PMove a, u ~ VUnit a, VUnit a ~ HUnit a) => Point2 u -> a -> a
at (P2 x y) p = pmove x y p


-- | Stack the pictures using 'over'.
stack :: (PEmpty a, Composite a) => [a] -> a
stack = foldl' over pempty


-- | The (one-dimensional) point midway between the left bound
-- and the right bound.
hcenter :: (Horizontal a, Fractional (HUnit a)) => a -> HUnit a
hcenter a = leftBound a + 0.5 * (rightBound a - leftBound a)


-- | The (one-dimensional) point midway between the left bound
-- and the right bound.
vcenter :: (Vertical a, Fractional (VUnit a)) => a -> VUnit a
vcenter a = bottomBound a + 0.5 * (topBound a - bottomBound a)

center :: (Horizontal a, Vertical a, Fractional u, 
           u ~ HUnit a, HUnit a ~ VUnit a)
       => a -> Point2 u
center a = P2 (hcenter a) (vcenter a)



(-@-) :: (Horizontal a, Vertical a, Composite a, PMove a, Fractional u, 
             u ~ HUnit a, HUnit a ~ VUnit a)
         => a -> a -> a
p1 -@- p2 = over p1 (pmove x y p2) where
  x = hcenter p1 - hcenter p2
  y = vcenter p1 - vcenter p2

stackCenter :: (PEmpty a, Horizontal a, Vertical a, Composite a, 
                PMove a, Fractional u,
                u ~ HUnit a, HUnit a ~ VUnit a)
            => [a] -> a
stackCenter = foldl' (-@-) pempty
