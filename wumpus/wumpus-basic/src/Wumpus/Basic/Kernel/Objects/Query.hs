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
    Info(..)
  
  , Query
  , LocQuery
  , LocThetaQuery

  , makeQuery
  , makeLocQuery
  , makeLocThetaQuery

  , promoteQuery_lq
  , promoteQuery_ltq
  , promoteLocQuery_ltq

  , cfQuery
  , cfLocQuery
  , cfLocThetaQuery

  , runQuery

  ) where

import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


class Info m where
  info :: (DrawingContext -> a) -> m a

newtype Query a = Query { getQuery :: DrawingContext -> a }


type LocQuery u a = Query (Point2 u -> a)

type LocThetaQuery u a = Query (Point2 u -> Radian -> a)



instance Functor Query where
  fmap f mf = Query $ \ctx -> f $ getQuery mf ctx

instance Applicative Query where
  pure a    = Query $ \_ -> a
  mf <*> ma = Query $ \ctx -> 
                let f = getQuery mf ctx
                    a = getQuery ma ctx
                in f a

instance Monad Query where
  return a  = Query $ \_   -> a
  ma >>= k  = Query $ \ctx -> let a = getQuery ma ctx in (getQuery . k) a ctx
                



instance Info Query where
  info fn = Query $ \ctx -> fn ctx



makeQuery :: (DrawingContext -> a) 
          -> (a -> ans) 
          -> Query ans
makeQuery qy fn = Query $ \ctx -> let a = qy ctx in fn a


makeLocQuery :: (DrawingContext -> a) 
             -> (a -> Point2 u -> ans) 
             -> LocQuery u ans
makeLocQuery qy fn = Query $ \ctx -> let a = qy ctx in (\pt -> fn a pt)


makeLocThetaQuery :: (DrawingContext -> a) 
                  -> (a -> Point2 u -> Radian -> ans) 
                  -> LocThetaQuery u ans
makeLocThetaQuery qy fn = Query $ \ctx -> 
    let a = qy ctx in (\pt ang -> fn a pt ang)



promoteQuery_lq :: (Point2 u -> Query ans) -> LocQuery u ans
promoteQuery_lq qy = Query $ \ctx pt -> getQuery (qy pt) ctx

promoteLocQuery_ltq :: (Radian -> LocQuery u ans) -> LocThetaQuery u ans
promoteLocQuery_ltq qy = Query $ \ctx pt ang -> getQuery (qy ang) ctx pt

promoteQuery_ltq :: (Point2 u -> Radian -> Query ans) -> LocThetaQuery u ans
promoteQuery_ltq qy = Query $ \ctx pt ang -> getQuery (qy pt ang) ctx


-- | Note this is just @return@. 
--
cfQuery :: ans -> Query ans
cfQuery = return 

-- | /Context free/ version of makeLocQuery.
--
cfLocQuery :: (Point2 u -> ans) -> LocQuery u ans
cfLocQuery fn = Query $ \_ pt -> fn pt


-- | /Context free/ version of makeLocQuery.
--
cfLocThetaQuery :: (Point2 u -> Radian -> ans) -> LocThetaQuery u ans
cfLocThetaQuery fn = Query $ \_ pt ang -> fn pt ang 


runQuery :: Query ans -> DrawingContext -> ans
runQuery qy ctx = getQuery qy ctx


