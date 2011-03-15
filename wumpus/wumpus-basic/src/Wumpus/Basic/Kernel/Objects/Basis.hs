{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

  , Object(..)

  , Arg1
  , Arg2
  , Arg3
  , Answer
  , PromoteR1(..)
  , PromoteR2(..)
  , PromoteR3(..)
  , Lift0R1(..)
  , Lift0R2(..)
  , Lift1R2(..)

  , MoveStart(..)
  , MoveStartTheta(..)

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





infixl 1 `bind`



class Object t where 
  local_ctx     :: forall (r :: * -> *) (u :: *). 
                   (DrawingContext -> DrawingContext) -> t r u -> t r u

  ignoreAns     :: forall (r :: * -> *) (u :: *).  
                   t r u -> t UNil u

  replaceAns    :: forall (r1 :: * -> *) (r :: * -> *) (u :: *).  
                   r1 u  -> t r u -> t r1 u

  mapAns        :: forall (r1 :: * -> *) (r :: * -> *) (u :: *).  
                   (r u -> r1 u)  -> t r u -> t r1 u

  hyperlink     :: forall (r :: * -> *) (u :: *). 
                   XLink -> t r u -> t r u 

  decorate      :: forall (r :: * -> *) (u :: *). 
                   t r u -> t UNil u -> t r u

  annotate      :: forall (r :: * -> *) (u :: *). 
                   t r u -> (r u -> t UNil u) -> t r u

  bind          :: forall (r :: * -> *) (r1 :: * -> *) (u :: *). 
                   t r u -> (r u -> t r1 u) -> t r1 u

  unit          :: forall (r :: * -> *) (u :: *). 
                   r u -> t r u





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


type family Arg1 t1 t
type family Arg2 t1 t 
type family Arg3 t1 t

type family Answer t

class PromoteR1 t1 t where
  promoteR1 :: (Answer t ~ Answer t1, a ~ Arg1 t1 t) => (a -> t) -> t1

class PromoteR2 t1 t where
  promoteR2 :: (Answer t ~ Answer t1, a ~ Arg1 t1 t, b ~ Arg2 t1 t) 
            => (a -> b -> t) -> t1

class PromoteR3 t1 t where
  promoteR3 :: ( Answer s ~ Answer s1
               , a ~ Arg1 t1 t, b ~ Arg2 t1 t, c ~ Arg3 t1 t ) 
            => (a -> b -> c -> t) -> t1


class Lift0R1 t1 t where
  lift0R1 :: Answer t ~ Answer t1 => t -> t1

class Lift0R2 t2 t where
  lift0R2 :: Answer t ~ Answer t2 => t -> t2

class Lift1R2 t2 t1 where
  lift1R2 :: Answer t1 ~ Answer t2 => t1 -> t2





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

