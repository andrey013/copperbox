{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernal.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2010-2011
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
  
    DUnit

  -- * A semigroup class
  , OPlus(..)
  , oconcat
  , altconcat

  -- * A bifunctor class
  , Bimap(..)
  , replaceL
  , replaceR

  -- * Unit phantom types 
  , UNil(..)
  , UOne(..)

  -- * Unit interpretation with respect to the current Point size
  , InterpretUnit(..)
  , UnitConvert(..)
  , uconvertScalar
  , uconvertF
  , intraMapPoint


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

import Control.Applicative


type family DUnit m :: *

type instance DUnit (Point2 u)      = u
type instance DUnit (Vec2 u)        = u
type instance DUnit (Matrix3'3 u)   = u




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


instance OPlus a => OPlus (Const a b) where
  Const a0 `oplus` Const a1 = Const $ a0 `oplus` a1 


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
-- Simple objects wrapped with unit phatom type 


data UNil   u = UNil          deriving (Eq,Ord,Read,Show)

data UOne a u = UOne a        deriving (Eq,Ord,Read,Show)

instance Functor UNil where
  fmap _ UNil= UNil


instance Functor (UOne a) where
  fmap _ (UOne a) = UOne a




instance OPlus (UNil u) where
  _ `oplus` _ = UNil

instance OPlus a => OPlus (UOne a u) where
  UOne a `oplus` UOne b = UOne $ a `oplus` b


instance Rotate (UNil u) where
  rotate _ = id

instance Scale (UNil u) where
  scale _ _  = id 

instance RotateAbout (UNil u) where
  rotateAbout _ _ = id

instance Translate (UNil u) where
  translate _ _ = id 


--------------------------------------------------------------------------------
-- Interpreting units 

-- Units may or may not depend on current font size
--

class Num u => InterpretUnit u where
  normalize :: FontSize -> u -> Double
  dinterp   :: FontSize -> Double -> u

instance InterpretUnit Double where
  normalize _ = id
  dinterp   _ = id 

instance InterpretUnit Centimeter where
  normalize _ = toPsDouble 
  dinterp   _ = fromPsDouble



class UnitConvert t where
  uconvert :: (InterpretUnit u, InterpretUnit u1) => t u -> t u1




-- | Convert a scalar value from one unit to another.
--
uconvertScalar :: (InterpretUnit u, InterpretUnit u1) => FontSize -> u -> u1
uconvertScalar sz = dinterp sz . normalize sz

-- | Unit convert an object that gives access to its unit at the
-- Functor position.
--
-- In practive this will be \*all\* Image answers.
--
uconvertF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
          => FontSize -> t u -> t u1
uconvertF sz = fmap (uconvertScalar sz)



-- Helper for defining Affine instances. This function allows 
-- scaling etc to be applied on a Point coerced to a Double then
-- converted back to the original unit. Thus transformations can 
-- work in contextual units.
--
intraMapPoint :: InterpretUnit u 
              => FontSize -> (DPoint2 -> DPoint2) -> Point2 u -> Point2 u
intraMapPoint sz fn (P2 x y) = 
    let P2 x' y' = fn $ P2 (normalize sz x) (normalize sz y)
    in  P2 (dinterp sz x') (dinterp sz y')



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


