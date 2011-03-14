{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Experimental extras for BaseDefs...
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Basis
  (

    PointDisplace
  , ThetaDisplace
  , ThetaPointDisplace

  , LocalCtx(..)
  , Hyperlink(..)
  , UMonad(..)

  , MoveStart(..)
  , MoveStartTheta(..)
  , Annotate(..)
  , IgnoreAns(..)
  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core



-- | 'PointDisplace' is a type representing functions 
-- @from Point to Point@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type PointDisplace u = Point2 u -> Point2 u




-- | 'ThetaDisplace' is a type representing functions 
-- @from Radian to Radian@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type ThetaDisplace = Radian -> Radian


-- | 'ThetaPointDisplace' is a type representing functions 
-- @from Radian * Point to Point@.
--
-- It is useful for building arrowheads which are constructed 
-- with an implicit angle representing the direction of the line 
-- at the arrow tip.
--
type ThetaPointDisplace u = Radian -> Point2 u -> Point2 u





class LocalCtx t where
  local_ctx :: forall (r :: * -> *) (u :: *). 
               (DrawingContext -> DrawingContext) -> t r u -> t r u



class Hyperlink obj where
  hyperlink :: XLink -> obj -> obj


 

infixl 1 `bind`

-- | Monad with answer type parameterized by unit @u@.
--
class UMonad t where 
  bind  :: forall (r :: * -> *) (r1 :: * -> *) (u :: *). 
           t r u -> (r u -> t r1 u) -> t r1 u

  unit  :: forall (r :: * -> *) (u :: *). r u -> t r u


-- DESIGN NOTE:
--
-- class PromoteR1 t1 t | t1 -> t where
--   promoteR1 :: forall (r :: * -> *) (u :: *).  (Point2 u -> t r u) -> t1 r u
--
-- A class for /promote/ seems difficult - it is hard to 
-- generalize the @Point2 u@ being promoted, moving to this
-- formulation loses the relation on units.
-- 
-- class PromoteR1 t1 t a | t1 -> t a where
--
-- A PromotePoint class would be possible:
-- 
-- class PromotePoint t1 t | t1 -> t where
--   promotePoint :: forall (r :: * -> *) (u :: *).  (Point2 u -> t r u) -> t1 r u
--



class MoveStart t where
  moveStart :: forall (r :: * -> *) (u :: *).  
               PointDisplace u -> t r u -> t r u


-- | 'moveStartTheta' - move the start-point of an inclined 
-- object with the supplied displacement function.
--
-- 'moveStartThetaAngle' - change the inclination of an inclined
-- object with the supplied displacement function.
--
class MoveStartTheta t where
  moveStartTheta :: forall (r :: * -> *) (u :: *).  
                    ThetaPointDisplace u -> t r u -> t r u


  moveStartThetaAngle :: forall (r :: * -> *) (u :: *).  
                         ThetaDisplace -> t r u -> t r u


class Annotate t where 
  decorate  :: t r u -> t UNil u -> t r u
  annotate  :: t r u -> (r u -> t UNil u) -> t r u
    
class IgnoreAns t where 
  ignoreAns  :: forall (r :: * -> *) (u :: *).  t r u -> t UNil u

  replaceAns :: forall (r1 :: * -> *) (r :: * -> *) (u :: *).  
                r1 u  -> t r u -> t r1 u

  mapAns     :: forall (r1 :: * -> *) (r :: * -> *) (u :: *).  
                (r u -> r1 u)  -> t r u -> t r1 u