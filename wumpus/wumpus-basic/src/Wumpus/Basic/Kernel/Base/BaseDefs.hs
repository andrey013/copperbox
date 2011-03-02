{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernal.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- The elementary base types and classes.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.BaseDefs
  (

  -- * A semigroup class
    OPlus(..)
  , oconcat
  , altconcat

  -- * A bifunctor class
  , Bimap(..)
  , replaceL
  , replaceR

  -- * Alignment
  , HAlign(..)
  , VAlign(..)  

  -- * Cardinal (compass) positions
  , Cardinal(..)

  -- * Advance vector
  , AdvanceVec
  , advanceH
  , advanceV

  ) where

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

infixr 6 `oplus`

-- | A Semigroup class.
-- 
-- The perhaps unusual name is the TeX name for the circled plus 
-- glyph. It would be nice if there was a semigroup class in the
-- Haskell Base libraries...  
-- 
class OPlus t where
  oplus :: t -> t -> t

-- | 'oconcat' : @ list_head * [rest] -> Ans @
-- 
-- Semigroup version of @mconcat@ from the module @Data.Monoid@.
--
-- As a semigroup cannot build a zero value, /concat/ cannot 
-- handle the empty list. So to make 'oconcat' a safe function
-- the input list is already destructured by one cons cell.
-- 
-- Effectively this means that client code must handle the 
-- empty list case, before calling 'oconcat'.
-- 
oconcat :: OPlus t => t -> [t] -> t
oconcat t = step t
  where
    step ac []     = ac
    step ac (x:xs) = step (ac `oplus` x) xs



-- | 'altconcat' : @ alternative * [list] -> Ans@
-- 
-- 'altconcat' uses 'oplus' to create a summary value from a list
-- of values. 
--
-- When supplied the empty list 'altconcat' returns the supplied 
-- /alternative/ value. If the list is inhabited, the alternative
-- value is discarded.
--
-- This contrasts to 'oconcat' where the single value represents 
-- the head of an already destructured list.
-- 
altconcat :: OPlus a => a -> [a] -> a
altconcat _   (x:xs) = oconcat x xs
altconcat alt []     = alt


instance OPlus () where
  _ `oplus` _ = ()

instance OPlus (UNil u) where
  _ `oplus` _ = uNil

instance Ord u => OPlus (BoundingBox u) where
  oplus = boundaryUnion

instance OPlus Primitive where
  a `oplus` b = primGroup [a,b]

instance (OPlus a, OPlus b) => OPlus (a,b) where
  (a,b) `oplus` (m,n) = (a `oplus` m, b `oplus` n)

instance (OPlus a, OPlus b, OPlus c) => OPlus (a,b,c) where
  (a,b,c) `oplus` (m,n,o) = (a `oplus` m, b `oplus` n, c `oplus` o)

instance (OPlus a, OPlus b, OPlus c, OPlus d) => OPlus (a,b,c,d) where
  (a,b,c,d) `oplus` (m,n,o,p) = (oplus a m, oplus b n, oplus c o, oplus d p)



instance OPlus a => OPlus (r -> a) where
  f `oplus` g = \x -> f x `oplus` g x

-- The functional instance (r -> a) also covers (r1 -> r2 -> a),
-- (r1 -> r2 -> r3 -> a) etc.

instance Num u => OPlus (Vec2 u) where 
  oplus = (^+^)


--------------------------------------------------------------------------------

-- | A Bifunctor class.
-- 
-- Again, it would be nice if there was a Bifunctor class in the
-- Haskell Base libraries...  
-- 
class Bimap f where
  bimap     :: (a -> p) -> (b -> q) -> f a b -> f p q
  bimapL    :: (a -> p) -> f a b -> f p b
  bimapR    :: (b -> q) -> f a b -> f a q



instance Bimap (,) where
  bimap f g (a,b) = (f a, g b)
  bimapL f (a,b)  = (f a, b)
  bimapR g (a,b)  = (a, g b)

instance Bimap Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)

  bimapL f (Left a)  = Left (f a)
  bimapL _ (Right b) = Right b

  bimapR _ (Left a)  = Left a
  bimapR g (Right b) = Right (g b)

replaceL :: Bimap f => p -> f a b -> f p b
replaceL = bimapL . const

replaceR :: Bimap f => q -> f a b -> f a q
replaceR = bimapR . const


--------------------------------------------------------------------------------

-- Alignment

-- | Horizontal alignment - align to the top, center or bottom.
--
data HAlign = HTop | HCenter | HBottom
  deriving (Enum,Eq,Ord,Show)

-- | Vertical alignment - align to the left, center or bottom.
--
data VAlign = VLeft | VCenter | VRight
  deriving (Enum,Eq,Ord,Show)

--------------------------------------------------------------------------------

-- Compass positions

-- | An enumeratied type representing the compass positions.
--
data Cardinal = NORTH | NORTH_EAST | EAST | SOUTH_EAST 
              | SOUTH | SOUTH_WEST | WEST | NORTH_WEST
   deriving (Enum,Eq,Ord,Show) 

--------------------------------------------------------------------------------

-- | Advance vectors provide an idiom for drawing consecutive
-- graphics. PostScript uses them to draw left-to-right text - 
-- each character has an advance vector for the width and 
-- as characters are drawn they successively displace the start
-- point for the next character with their advance vector.
--
-- Type alias for Vec2.
--
type AdvanceVec u = Vec2 u


-- | Extract the horizontal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0. Ingoring it seems 
-- permissible when drawing text.
--
advanceH :: AdvanceVec u -> u
advanceH (V2 w _)  = w

-- | Extract the verticall component of an advance vector.
--
advanceV :: AdvanceVec u -> u
advanceV (V2 _ h)  = h


