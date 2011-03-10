{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.ImageBasis
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

module Wumpus.Basic.Kernel.Objects.ImageBasis
  (

    PointDisplace
  , ThetaDisplace
  , ThetaPointDisplace

  , Localize(..)
  , Hyperlink(..)
  , OpBind(..)
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





class Localize t where
  localize :: forall (r :: * -> *) (u :: *). 
              (DrawingContext -> DrawingContext) -> t r u -> t r u



class Hyperlink obj where
  hyperlink :: XLink -> obj -> obj



-- Whoa - bind probably not useful without return, but return 
-- is problemmatic as graphics are a semigroup not a monoid.
--

-- | Operator parameterized bind!


class OpBind t where 
  opbind  :: forall (r :: * -> *) (u :: *). 
             (r u -> r u -> r u) -> t r u -> (r u -> t r u) -> t r u


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
  ignoreAns  :: t r u -> t UNil u
  replaceAns :: r1 u  -> t r u -> t r1 u
