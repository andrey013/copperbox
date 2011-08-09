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
  
    MonUnit 


  -- * Unit phantom type
  , UNil(..)
  , ureturn

  -- * Non-contextual unit conversion.
  , ScalarUnit(..)

  -- * Unit interpretation with respect to the current Point size
  , InterpretUnit(..)
  , dinterpF
  , normalizeF
  , uconvert1
  , uconvertF

  , intraMapPoint
  , intraMapFunctor


  -- * KernChar
  , KernChar

  -- * Drawing paths
  , DrawStyle(..)

  -- * Drawing /layer/
  , ZDeco(..)  

  -- * Alignment
  , HAlign(..)
  , VAlign(..)  

  -- * Text height
  , TextHeight(..)

  -- * Cardinal (compass) positions
  , Cardinal(..)

  -- * Direction enumeration
  , Direction(..)
  

  -- * Misc
  , vsum
  , both

  ) where

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid





-- | Type family to access the unit parameter of a TraceDrawing
-- or a promoted TraceDrawingT transformer.
--
type family MonUnit m :: *



--------------------------------------------------------------------------------
-- Simple objects wrapped with unit phatom type 


-- | The empty data type - i.e. @()@ - wrapped with a phantom unit 
-- parameter.
--
data UNil   u = UNil          deriving (Eq,Ord,Read,Show)

type instance DUnit (UNil u) = u

instance Functor UNil where
  fmap _ UNil= UNil


instance Monoid (UNil u) where
  mempty        = UNil
  _ `mappend` _ = UNil




instance Rotate (UNil u) where
  rotate _              = id

instance RotateAbout (UNil u) where
  rotateAbout _ _       = id

instance Scale (UNil u) where
  scale _ _             = id

instance Translate (UNil u) where
  translate _ _         = id

-- | Return a 'UNil' rather than @()@ at the end of sequence of
-- monadic commands.
--
-- Many Wumpus objects are usefully constructed in the 
-- @do-notation@, but due to the need to type their unit
-- must finish the do-block with:
--
-- > ureturn
-- 
-- or:
-- 
-- return UNil
--
-- rather than:
--
-- > return ()
--
--
ureturn :: Monad m => m (UNil u)
ureturn = return UNil

--------------------------------------------------------------------------------
-- Non-contextual units

class ScalarUnit a where
  fromPsPoint :: Double -> a 
  toPsPoint   :: a -> Double

instance ScalarUnit Double where
  fromPsPoint = id
  toPsPoint   = id 



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

instance InterpretUnit AfmUnit where
  normalize sz = afmValue sz 
  dinterp   sz = afmUnit sz


-- | 'dinterp' an object that gives access to its unit at the 
-- functor position.
--
dinterpF :: (Functor t, InterpretUnit u) => FontSize -> t Double -> t u
dinterpF sz = fmap (dinterp sz)


-- | 'normalize' an object that gives access to its unit at the 
-- functor position.
--
normalizeF :: (Functor t, InterpretUnit u) => FontSize -> t u -> t Double
normalizeF sz = fmap (normalize sz)


-- | Convert a scalar value from one unit to another.
--
uconvert1 :: (InterpretUnit u, InterpretUnit u1) => FontSize -> u -> u1
uconvert1 sz = dinterp sz . normalize sz

-- | Unit convert an object that gives access to its unit at the
-- Functor position.
--
-- In practive this will be \*all\* Image answers.
--
uconvertF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
          => FontSize -> t u -> t u1
uconvertF sz = fmap (uconvert1 sz)



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



-- Helper for defining Affine instances. This function allows 
-- scaling etc to be applied on a Point coerced to a Double then
-- converted back to the original unit. Thus transformations can 
-- work in contextual units.
--
intraMapFunctor :: (Functor f, InterpretUnit u)
                => FontSize -> (f Double -> f Double) -> f u -> f u
intraMapFunctor sz fn ma = dinterpF sz $ fn $ normalizeF sz ma


--------------------------------------------------------------------------------
-- KernChar


-- | Unit parametric version of KerningChar from Wumpus-Core.
--
type KernChar u = (u,EscapedChar)


--------------------------------------------------------------------------------
-- Drawing closed paths


-- | Draw closed paths. 
-- 
-- > FILL 
--
-- > STROKE
--
-- > FILL_STROKE - the path is filled and its edge is stroked.
--
data DrawStyle = FILL | STROKE | FILL_STROKE
  deriving (Bounded,Enum,Eq,Ord,Show)



-- | Decorating with resepct to the Z-order 
-- 
-- > SUPERIOR - in front. 
--
-- > ANTERIOR - behind.
--
data ZDeco = SUPERIOR | ANTERIOR
  deriving (Bounded,Enum,Eq,Ord,Show)


--------------------------------------------------------------------------------

-- Alignment

-- | Horizontal alignment - align to the top, center or bottom.
--
data HAlign = HALIGN_TOP | HALIGN_CENTER | HALIGN_BASE
  deriving (Enum,Eq,Ord,Show)

-- | Vertical alignment - align to the left, center or bottom.
--
data VAlign = VALIGN_LEFT | VALIGN_CENTER | VALIGN_RIGHT
  deriving (Enum,Eq,Ord,Show)


--------------------------------------------------------------------------------
-- Text height


-- | Wumpus distinguishes two use-cases for displaying vertically 
-- centered text.
-- 
-- Arbitrary text that is expected to contain lower case letters 
-- with descenders, show take the vertical center as the mid-point 
-- between the cap height and the descender depth.
--
-- Unfortunately, including the descender depth can produce 
-- unbalanced results for text which is not expected to have 
-- descenders (e.g. numbers within a bordered box), visually this 
-- makes the center too high.
-- 
data TextHeight = JUST_CAP_HEIGHT | CAP_HEIGHT_PLUS_DESCENDER
  deriving (Enum,Eq,Ord,Show)



--------------------------------------------------------------------------------

-- Compass positions

-- | An enumeratied type representing the compass positions.
--
data Cardinal = NORTH | NORTH_EAST | EAST | SOUTH_EAST 
              | SOUTH | SOUTH_WEST | WEST | NORTH_WEST
   deriving (Enum,Eq,Ord,Show) 


-- | An enumerated type representing horizontal and vertical 
-- directions.
data Direction = UP | DOWN | LEFT | RIGHT
   deriving (Enum,Eq,Ord,Show) 




-- | Sum a list of Vectors.
--
-- Note - this function is a candidate to go in Wumpus-Core, but
-- it will be added when there is an intrinsic reason to to update
-- Core (bug fix, or API change).
--
vsum :: Num u => [Vec2 u] -> Vec2 u
vsum [] = V2 0 0
vsum (v:vs) = go v vs
  where
    go a []     = a
    go a (b:bs) = go (a ^+^ b) bs


-- | Applicative /both/ - run both computations return the pair
-- of the the answers.
--
both :: Applicative f => f a -> f b -> f (a,b)
both fa fb = (,) <$> fa <*> fb

