{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Query
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- TEMP!
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Query
  (


    Query
  , LocQuery
  , LocThetaQuery

  , makeQuery
  , makeLocQuery
  , makeLocThetaQuery

  , cfLocQuery
  , runQuery
  , runLocQuery
  , runLocThetaQuery

  ) where

import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



newtype Query a = Query { getQuery :: DrawingContext -> a }


newtype LocQuery u a = LocQuery { 
    getLocQuery :: DrawingContext -> Point2 u -> a }

newtype LocThetaQuery u a = LocThetaQuery { 
    getLocThetaQuery :: DrawingContext -> Point2 u -> Radian -> a }



makeQuery :: (DrawingContext -> a) 
          -> (a -> ans) 
          -> Query ans
makeQuery qry fn = Query $ \ctx -> let a = qry ctx in fn a

makeLocQuery :: (DrawingContext -> a) 
             -> (a -> Point2 u -> ans) 
             -> LocQuery u ans
makeLocQuery qry fn = LocQuery $ \ctx pt -> let a = qry ctx in fn a pt


makeLocThetaQuery :: (DrawingContext -> a) 
             -> (a -> Point2 u -> Radian -> ans) 
             -> LocThetaQuery u ans
makeLocThetaQuery qry fn = LocThetaQuery $ \ctx pt ang -> 
    let a = qry ctx in fn a pt ang


-- | /Context-free/ LocInfo constructor.
--
cfLocQuery  :: (Point2 u -> a) -> LocQuery u a
cfLocQuery fn = LocQuery $ \_ pt -> fn pt


runQuery :: Query ans -> DrawingContext -> ans
runQuery qry ctx = getQuery qry ctx

runLocQuery :: LocQuery u ans -> DrawingContext -> Point2 u -> ans
runLocQuery qry ctx pt = getLocQuery qry ctx pt

runLocThetaQuery :: LocThetaQuery u ans 
                 -> DrawingContext -> Point2 u -> Radian -> ans
runLocThetaQuery qry ctx pt ang = getLocThetaQuery qry ctx pt ang



