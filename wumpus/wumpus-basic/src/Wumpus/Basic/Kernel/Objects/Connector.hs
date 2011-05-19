{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Connector
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ConnImage and ConnGraphic types - these are functional types
-- from the DrawingContext plus start point and end point to a 
-- graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Connector
   (
     ConnectorImage
   , ConnectorGraphic

   , DConnectorImage
   , DConnectorGraphic

   , ConnectorQuery

   , runConnectorImage
   , runConnectorQuery
   , connect

   , promoteConn
   , applyConn
   , qpromoteConn
   , qapplyConn
   , zapConnectorQuery

   , emptyConnectorImage

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Basis

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid


-- | ConnectorImage - function from DrawingContext and start and 
-- end points to a polymorphic /answer/ and a graphic /primitive/.
--
newtype ConnectorImage u a = ConnectorImage { 
          getConnectorImage :: Point2 u -> Point2 u -> Image u a }


type instance DUnit (ConnectorImage u a) = u



type ConnectorGraphic u = ConnectorImage u (UNil u)

-- | Type specialized version of 'ConnectorImage'.
--
type DConnectorImage a        = ConnectorImage Double a

-- | Type specialized version of 'ConnectorGraphic'.
--
type DConnectorGraphic        = ConnectorGraphic Double 


newtype ConnectorQuery u a = ConnectorQuery { 
          getConnectorQuery :: Point2 u -> Point2 u -> Query u a }


-- Functor 

instance Functor (ConnectorImage u) where
  fmap f ma = ConnectorImage $ \p0 p1 -> fmap f $ getConnectorImage ma p0 p1

instance Functor (ConnectorQuery u) where
  fmap f ma = ConnectorQuery $ \p0 p1 -> fmap f $ getConnectorQuery ma p0 p1


-- Applicative

instance Applicative (ConnectorImage u) where
  pure a    = ConnectorImage $ \_  _  -> pure a
  mf <*> ma = ConnectorImage $ \p0 p1 -> 
                getConnectorImage mf p0 p1 <*> getConnectorImage ma p0 p1

instance Applicative (ConnectorQuery u) where
  pure a    = ConnectorQuery $ \_  _  -> pure a
  mf <*> ma = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery mf p0 p1 <*> getConnectorQuery ma p0 p1


-- Monad 

instance Monad (ConnectorImage u) where
  return a  = ConnectorImage $ \_  _  -> return a
  ma >>= k  = ConnectorImage $ \p0 p1 -> 
                getConnectorImage ma p0 p1 >>= \ans -> 
                getConnectorImage (k ans) p0 p1


instance Monad (ConnectorQuery u) where
  return a  = ConnectorQuery $ \_  _  -> return a
  ma >>= k  = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery ma p0 p1 >>= \ans -> 
                getConnectorQuery (k ans) p0 p1


-- Monoid

instance Monoid a => Monoid (ConnectorImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorImage $ \p0 p1 -> 
                      getConnectorImage ma p0 p1 
                        `mappend` getConnectorImage mb p0 p1 


instance Monoid a => Monoid (ConnectorQuery u a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorQuery $ \p0 p1 -> 
                      getConnectorQuery ma p0 p1 
                        `mappend` getConnectorQuery mb p0 p1 



-- DrawingCtxM

instance DrawingCtxM (ConnectorImage u) where
  askDC           = ConnectorImage $ \_  _  -> askDC
  asksDC fn       = ConnectorImage $ \_  _  -> asksDC fn
  localize upd ma = ConnectorImage $ \p0 p1 -> 
                      localize upd (getConnectorImage ma p0 p1)

instance DrawingCtxM (ConnectorQuery u) where
  askDC           = ConnectorQuery $ \_  _  -> askDC
  asksDC fn       = ConnectorQuery $ \_  _  -> asksDC fn
  localize upd ma = ConnectorQuery $ \p0 p1 -> 
                      localize upd (getConnectorQuery ma p0 p1)


  


instance Decorate ConnectorImage where
  decorate zo ma mz = ConnectorImage $ \p0 p1 -> 
    decorate zo (getConnectorImage ma p0 p1) (getConnectorImage mz p0 p1)

  elaborate zo ma f = ConnectorImage $ \p0 p1 -> 
    elaborate zo (getConnectorImage ma p0 p1) 
                 (\a -> getConnectorImage (f a) p0 p1)

  obliterate ma mz = ConnectorImage $ \p0 p1 -> 
    getConnectorImage ma p0 p1 `obliterate` getConnectorImage mz p0 p1

  hyperlink xl ma = ConnectorImage $ \p0 p1 -> 
    hyperlink xl $ getConnectorImage ma p0 p1



runConnectorImage :: Point2 u -> Point2 u 
                  -> DrawingContext -> ConnectorImage u a 
                  -> PrimW u a
runConnectorImage p0 p1 ctx mf = runImage ctx (getConnectorImage mf p0 p1)


runConnectorQuery :: Point2 u -> Point2 u 
                  -> DrawingContext -> ConnectorQuery u a 
                  -> a
runConnectorQuery p0 p1 ctx mf = runQuery ctx (getConnectorQuery mf p0 p1)


connect :: Point2 u -> Point2 u -> ConnectorImage u a -> Image u a
connect p0 p1 mf = getConnectorImage mf p0 p1




promoteConn :: (Point2 u -> Point2 u -> Image u a) -> ConnectorImage u a
promoteConn fn       = ConnectorImage $ \p0 p1 -> fn p0 p1

applyConn :: ConnectorImage u a -> Point2 u -> Point2 u -> Image u a
applyConn mf p0 p1   = getConnectorImage mf p0 p1


qpromoteConn :: (Point2 u -> Point2 u -> Query u a) -> ConnectorQuery u a
qpromoteConn fn      = ConnectorQuery $ \p0 p1 -> fn p0 p1

qapplyConn :: ConnectorQuery u a -> Point2 u -> Point2 u -> Query u a
qapplyConn mf p0 p1   = getConnectorQuery mf p0 p1


-- | \"zero-apply\" a Connector.
--
zapConnectorQuery :: ConnectorQuery u a -> Point2 u -> Point2 u -> Image u a
zapConnectorQuery mq p0 p1  = askDC >>= \ctx -> 
    let a = runConnectorQuery p0 p1 ctx mq in return a



instance UConvert ConnectorImage where
  uconvF = uconvConnectorImageF
  uconvZ = uconvConnectorImageZ


-- | Use this to convert 'ConnectorGraphic' or 'ConnectorImage' 
-- with Functor answer.
--
uconvConnectorImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                     => ConnectorImage u (t u) -> ConnectorImage u1 (t u1)
uconvConnectorImageF ma = ConnectorImage $ \p0 p1 -> 
    getFontSize >>= \sz -> 
    let p0u = uconvertF sz p0
        p1u = uconvertF sz p1
    in uconvImageF $ getConnectorImage ma p0u p1u




-- | Use this to convert 'ConnectorImage' with unit-less answer.
--
uconvConnectorImageZ :: (InterpretUnit u, InterpretUnit u1) 
                     => ConnectorImage u a -> ConnectorImage u1 a
uconvConnectorImageZ ma = ConnectorImage $ \p0 p1 -> 
    getFontSize >>= \sz -> 
    let p0u = uconvertF sz p0
        p1u = uconvertF sz p1
    in uconvImageZ $ getConnectorImage ma p0u p1u

-- | Having /empty/ at the specific 'ConnectorImage' type is useful.
-- 
emptyConnectorImage :: Monoid a => ConnectorImage u a
emptyConnectorImage = mempty


--------------------------------------------------------------------------------



--
-- Design note - potentially there are no useful combining 
-- operators on Connectors (!).
--
-- Division - i.e. splitting a path at points between the start 
-- and end - seems a more obvious operation on connector paths 
-- than combination. See the ConnectorPath operations in 
-- Wumpus-Drawing for some examples.
--

