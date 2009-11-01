{-# LANGUAGE TypeFamilies               #-}
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

module Wumpus.Core.PictureLanguage where

import Wumpus.Core.Geometry ( Point2(..) )

import Data.List ( foldl' )

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

-- Basically monoid, but implementaions should move neither 
-- picture.
class Composite a where
  composite :: a -> a -> a
  cempty    :: a 

-- Move in 2D
class (Vertical a, Horizontal a) => PMove a where
  pmove   :: HUnit a -> VUnit a -> a -> a


infixr 5 <//>
infixr 6 <>

-- | Horizontal composition - place @b@ at the right of @a@
(<>) :: (Horizontal a, Composite a, Num u, u ~ HUnit a) => a -> a -> a
a <> b = composite a (moveH disp b) where disp = rightBound a - leftBound b 


-- | Vertical composition - place @b@ below @a@.
(<//>) :: (Vertical a, Composite a, Num u, u ~ VUnit a) => a -> a -> a
a <//> b = composite a (moveV disp b) where disp = bottomBound a - topBound b 



at :: (PMove a, u ~ VUnit a, VUnit a ~ HUnit a) => Point2 u -> a -> a
at (P2 x y) p = pmove x y p


-- | Stack the pictures 
stack :: Composite a => [a] -> a
stack = foldl' composite cempty

