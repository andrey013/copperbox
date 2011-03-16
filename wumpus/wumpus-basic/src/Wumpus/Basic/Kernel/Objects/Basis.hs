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

  , ArgDiff
  , Answer
  , PromoteR1(..)
  , PromoteR2(..)
  , PromoteR3(..)
  , Lift0R1(..)
  , Lift0R2(..)
  , Lift1R2(..)
  , Lift0R3(..)
  , Lift1R3(..)
  , Lift2R3(..)


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

  clipObject    :: forall (r :: * -> *) (u :: *). 
                   PrimPath -> t r u -> t r u 

  decorate      :: forall (r :: * -> *) (u :: *). 
                   t r u -> t UNil u -> t r u

  annotate      :: forall (r :: * -> *) (u :: *). 
                   t r u -> (r u -> t UNil u) -> t r u

  bind          :: forall (r :: * -> *) (r1 :: * -> *) (u :: *). 
                   t r u -> (r u -> t r1 u) -> t r1 u

  unit          :: forall (r :: * -> *) (u :: *). 
                   r u -> t r u








type family Answer t

type family ArgDiff t1 t




class PromoteR1 t1 t where
  promoteR1 :: (Answer t ~ Answer t1, a ~ ArgDiff t1 t) => (a -> t) -> t1

class PromoteR2 t2 t where
  promoteR2 :: (Answer t ~ Answer t2, (a,b) ~ ArgDiff t2 t) 
            => (a -> b -> t) -> t2

class PromoteR3 t3 t where
  promoteR3 :: (Answer t ~ Answer t3, (a,b,c) ~ ArgDiff t3 t) 
            => (a -> b -> c -> t) -> t3


-- | Lift from arity 0 to arity 1.
--
class Lift0R1 t1 t where
  lift0R1 :: Answer t ~ Answer t1 => t -> t1

-- | Lift from arity 0 to arity 2.
--
class Lift0R2 t2 t where
  lift0R2 :: Answer t ~ Answer t2 => t -> t2

-- | Lift from arity 1 to arity 2.
--
class Lift1R2 t2 t1 where
  lift1R2 :: Answer t1 ~ Answer t2 => t1 -> t2

-- | Lift from arity 0 to arity 3.
--
class Lift0R3 t3 t where
  lift0R3 :: Answer t ~ Answer t3 => t -> t3

-- | Lift from arity 1 to arity 3.
--
class Lift1R3 t3 t1 where
  lift1R3 :: Answer t1 ~ Answer t3 => t1 -> t3

-- | Lift from arity 2 to arity 3.
--
class Lift2R3 t3 t2 where
  lift2R3 :: Answer t2 ~ Answer t3 => t2 -> t3




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



