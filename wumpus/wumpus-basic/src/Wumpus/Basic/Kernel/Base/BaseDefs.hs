{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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

  -- * A bifunctor class
  , Bimap(..)
  , replaceL
  , replaceR

  -- * Alignment
  , HAlign(..)
  , VAlign(..)  

  -- * Advance vector
  , AdvanceVec
  , advanceH
  , advanceV


  -- * Monadic drawing
  , MonUnit

  , PointSupplyM(..)

  ) where

import Wumpus.Core                              -- package: wumpus-core


infixr 6 `oplus`

-- | A Semigroup class.
-- 
-- The perhaps unusual name is the TeX name for the circled plus 
-- glyph. It would be nice if there was a semigroup class in the
-- Haskell Base libraries...  
-- 
class OPlus t where
  oplus :: t -> t -> t

oconcat :: OPlus t => t -> [t] -> t
oconcat t = step t
  where
    step ac []     = ac
    step ac (x:xs) = step (ac `oplus` x) xs



instance OPlus () where
  _ `oplus` _ = ()

instance OPlus (UNil u) where
  _ `oplus` _ = uNil

instance Ord u => OPlus (BoundingBox u) where
  oplus = boundaryUnion

instance OPlus (Primitive u) where
  a `oplus` b = primGroup [a,b]

instance (OPlus a, OPlus b) => OPlus (a,b) where
  (a,b) `oplus` (a',b') = (a `oplus` a', b `oplus` b')


instance OPlus a => OPlus (r -> a) where
  f `oplus` g = \x -> f x `oplus` g x

-- The functional instance (r -> a) also covers (r1 -> r2 -> a),
-- (r1 -> r2 -> r3 -> a) etc.

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




--------------------------------------------------------------------------------
-- Monadic drawing

-- | DUnit is always for fully saturated type constructors, so 
-- (seemingly) an equivalent type family is needed for monads.

type family MonUnit m :: * 


-- | A monad that supplies points, e.g. a turtle monad. 
--
-- \*\* WARNING \*\* - the idea behind this class is somewhat
-- half-baked. It may be revised or even dropped in subsequent
-- versions of Wumpus-Basic.
--
class Monad m => PointSupplyM (m :: * -> *) where
  position :: MonUnit m ~ u => m (Point2 u)
