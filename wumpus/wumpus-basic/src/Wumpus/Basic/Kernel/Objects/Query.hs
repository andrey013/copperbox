{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
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
-- Queries to build drawing objects.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Query
  (
  
    Query
  , LocQuery
  , LocThetaQuery

  , BindQuery1(..)
  , BindQuery2(..)
  , BindQuery3(..)

  , makeQuery
  , makeLocQuery
  , makeLocThetaQuery

  , promoteQ1
  , promoteQ2
  , applyQ1
  , applyQ2
 
  , promoteQuery_lq
  , promoteQuery_ltq
  , promoteLocQuery_ltq

  , cfQuery
  , cfLocQuery
  , cfLocThetaQuery

  , runQuery

  ) where

import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



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
                

instance DrawingCtxM Query where
  askDC          = Query $ \ctx -> ctx
  localize fn qy = Query $ \ctx -> getQuery qy (fn ctx)



infixr 1 &=>, &==>, &===>

class BindQuery1 t where
  (&=>) :: Query ans -> (ans -> t) -> t


class BindQuery2 t1 t where
  (&==>)  :: (Answer t ~ Answer t1, r1 ~ Arg1 t1 t) =>
             Query (r1 -> ans) -> (ans -> t) -> t1


class BindQuery3 t2 t where
  (&===>) :: (Answer t ~ Answer t2, r1 ~ Arg1 t2 t, r2 ~ Arg2 t2 t) =>
             Query (r1 -> r2 -> ans) -> (ans -> t) -> t2





makeQuery :: Query a 
          -> (a -> ans) 
          -> Query ans
makeQuery qy fn = Query $ \ctx -> let a = runQuery qy ctx in fn a


makeLocQuery :: Query a
             -> (a -> Point2 u -> ans) 
             -> LocQuery u ans
makeLocQuery qy fn = Query $ \ctx -> 
    let a = runQuery qy ctx in (\pt -> fn a pt)


makeLocThetaQuery :: Query a
                  -> (a -> Point2 u -> Radian -> ans) 
                  -> LocThetaQuery u ans
makeLocThetaQuery qy fn = Query $ \ctx -> 
    let a = runQuery qy ctx in (\pt ang -> fn a pt ang)



promoteQ1 :: (a -> Query ans) -> Query (a -> ans)
promoteQ1 qy = Query $ \ctx a -> getQuery (qy a) ctx

promoteQ2 :: (a -> b -> Query ans) -> Query (a -> b -> ans)
promoteQ2 qy = Query $ \ctx a b -> getQuery (qy a b) ctx

applyQ1 :: Query (a -> ans) -> a -> Query ans
applyQ1 mf a = fmap ($ a) mf 

applyQ2 :: Query (a -> b -> ans) -> a -> b -> Query ans
applyQ2 mf a b = fmap (\f -> f a b) mf 


-- OLD names ...
promoteQuery_lq :: (Point2 u -> Query ans) -> LocQuery u ans
promoteQuery_lq = promoteQ1

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


