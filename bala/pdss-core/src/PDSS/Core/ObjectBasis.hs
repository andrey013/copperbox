{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.ObjectBasis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - .
--
--------------------------------------------------------------------------------


module PDSS.Core.ObjectBasis
  ( 

    Obj(..)

  , Image   
  , Query
  , Graphic

  , runImage
  , runQuery
  , liftQuery
  , primElement
  , primObject

  , LocImage
  , LocQuery
  , LocGraphic

  , runLocImage
  , runLocQuery

  , promoteLoc
  , applyLoc
  , at


  , Port(..)
  , ConnectorImage
  , ConnectorQuery
  , ConnectorGraphic

  , runConnectorImage
  , runConnectorQuery

  , connector
  , promoteConn
  
  , HasID(..)

  , HasIn0
  , HasIn1
  , HasIn2
  , HasOut0
  , HasOut1
  , HasOut2

  , inport0
  , inport1
  , inport2
  , outport0
  , outport1
  , outport2

  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.InternalTypes
import PDSS.Core.Context
import PDSS.Core.Utils.FormatCombinators

import Control.Applicative
import Data.Monoid




type PrimResult a = (a,GenSt,Primitive)


-- | Bang is defined by size which is the same length in both directions.
-- 
data Obj = Obj
    { obj_id   :: Int
    , obj_bb   :: BoundingBox
    }
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- Images


newtype Image a = Image { 
    getImage :: PdContext -> GenSt -> (a, GenSt, Primitive) }


-- | Note - queries don\'t have access to the object counter.
--
-- This means we can\'t write an equivalent of @stripImage@ from
-- Wumpus.
--
newtype Query a = Query { getQuery :: PdContext -> a }


-- Functor

instance Functor Image where
  fmap f ma = Image $ \r s -> let (a,s1,w1) = getImage ma r s in (f a, s1, w1)

instance Functor Query where
  fmap f ma = Query $ \r -> f $ getQuery ma r


-- Applicative

instance Applicative Image where
  pure a    = Image $ \_ s  -> (a,s,mempty)
  mf <*> ma = Image $ \r s -> let (f,s1,w1) = getImage mf r s
                                  (a,s2,w2) = getImage ma r s1
                              in (f a, s2, w1 `mappend` w2)


instance Applicative Query where
  pure a    = Query $ \_ -> a
  mf <*> ma = Query $ \r -> let f = getQuery mf r 
                                a = getQuery ma r
                            in f a


-- Monad

instance Monad Image where
  return a = Image $ \_ s  -> (a,s,mempty)
  ma >>= k = Image $ \r s -> let (a,s1,w1) = getImage ma r s
                                 (b,s2,w2) = getImage (k a) r s1
                             in (b,s2,w1 `mappend` w2)

instance Monad Query where
  return a = Query $ \_   -> a
  ma >>= k = Query $ \r -> let a = getQuery ma r in getQuery (k a) r


-- Monoid

instance Monoid a => Monoid (Image a) where
  mempty          = pure mempty
  ma `mappend` mb = Image $ \r s -> 
                      let (a,s1,w1) = getImage ma r s
                          (b,s2,w2) = getImage mb r s1
                      in (a `mappend` b, s2, w1 `mappend` w2)

instance Monoid a => Monoid (Query a) where
  mempty          = pure mempty
  ma `mappend` mb = Query $ \r -> 
                      getQuery ma r `mappend` getQuery mb r


-- ContextM

instance ContextM Image  where
  askCtx          = Image $ \r s -> (r, s, mempty)
  asksCtx fn      = Image $ \r s -> (fn r, s, mempty)
  localize upd ma = Image $ \r s -> getImage ma (upd r) s


instance ContextM Query where
  askCtx          = Query $ \r -> r
  asksCtx fn      = Query $ \r -> (fn r)
  localize upd ma = Query $ \r -> getQuery ma (upd r)


type Graphic = Image ()



runImage :: PdContext -> GenSt -> Image a -> PrimResult a
runImage ctx st ma = getImage ma ctx st


runQuery :: PdContext -> Query a -> a
runQuery ctx ma = getQuery ma ctx



-- | Turn a 'Query' into an 'Image' without graphic content.
--
liftQuery :: Query a -> Image a
liftQuery ma = askCtx >>= \r -> return (runQuery r ma)


primElement :: Doc -> Graphic
primElement d1 = Image $ \_ s -> ((),s, primitive d1)

primObject :: Doc -> (Int -> a) -> Image a
primObject d1 f = Image $ \_ s -> let (i,s1) = incrSt s in (f i,s1, primitive d1)


--------------------------------------------------------------------------------
-- LocImage


-- | 'LocImage' - function from  start point and 
-- DrawingContext to a polymorphic /answer/ and a graphic 
-- /primitive/.
--
newtype LocImage a = LocImage { getLocImage :: Point -> Image a }

newtype LocQuery a = LocQuery { getLocQuery :: Point -> Query a }


type LocGraphic = LocImage ()

-- Functor

instance Functor LocImage where
  fmap f ma = LocImage $ \pt -> fmap f $ getLocImage ma pt

instance Functor LocQuery where
  fmap f ma = LocQuery $ \pt -> fmap f $ getLocQuery ma pt


-- Applicative

instance Applicative LocImage where
  pure a    = LocImage $ \_  -> pure a
  mf <*> ma = LocImage $ \pt -> getLocImage mf pt <*> getLocImage ma pt


instance Applicative LocQuery where
  pure a    = LocQuery $ \_  -> pure a
  mf <*> ma = LocQuery $ \pt -> getLocQuery mf pt <*> getLocQuery ma pt

-- Monad

instance Monad LocImage where
  return a  = LocImage $ \_  -> return a
  ma >>= k  = LocImage $ \pt -> getLocImage ma pt >>= \ans -> 
                                  getLocImage (k ans) pt

instance Monad LocQuery where
  return a  = LocQuery $ \_  -> return a
  ma >>= k  = LocQuery $ \pt -> getLocQuery ma pt >>= \ans -> 
                                  getLocQuery (k ans) pt

-- Monoid

instance Monoid a => Monoid (LocImage a) where
  mempty          = pure mempty
  ma `mappend` mb = LocImage $ \pt -> 
                      getLocImage ma pt `mappend` getLocImage mb pt 


instance Monoid a => Monoid (LocQuery a) where
  mempty          = pure mempty
  ma `mappend` mb = LocQuery $ \pt -> 
                      getLocQuery ma pt `mappend` getLocQuery mb pt 


-- ContextM

instance ContextM LocImage where
  askCtx          = LocImage $ \_  -> askCtx
  asksCtx fn      = LocImage $ \_  -> asksCtx fn
  localize upd ma = LocImage $ \pt -> localize upd (getLocImage ma pt)

instance ContextM LocQuery where
  askCtx          = LocQuery $ \_  -> askCtx
  asksCtx fn      = LocQuery $ \_  -> asksCtx fn
  localize upd ma = LocQuery $ \pt -> localize upd (getLocQuery ma pt)



runLocImage :: PdContext -> GenSt -> Point -> LocImage a -> PrimResult a
runLocImage ctx st pt ma = runImage ctx st $ getLocImage ma pt

runLocQuery :: PdContext -> Point -> LocQuery a -> a
runLocQuery ctx pt ma = runQuery ctx $ getLocQuery ma pt


promoteLoc ::  (Point -> Image a) -> LocImage a
promoteLoc k = LocImage $ \pt -> k pt 

applyLoc :: LocImage a -> Point -> Image a
applyLoc ma pt = getLocImage ma pt


infixr 1 `at`

-- | Downcast a 'LocImage' function by applying it to the supplied 
-- point, making an 'Image'. 
-- 
-- > infixr 1 `at`
-- 
at :: LocImage a -> Point -> Image a
at = applyLoc



--------------------------------------------------------------------------------
-- Connectors

data Port = Port { parent_obj :: Int, port_num :: Int }
  deriving (Eq,Ord,Show)

newtype ConnectorImage a = ConnectorImage { 
      getConnectorImage :: Port -> Port -> Image a }

newtype ConnectorQuery a = ConnectorQuery { 
      getConnectorQuery :: Port -> Port -> Query a }

type ConnectorGraphic = ConnectorImage ()


-- Functor 

instance Functor ConnectorImage where
  fmap f ma = ConnectorImage $ \p0 p1 -> fmap f $ getConnectorImage ma p0 p1

instance Functor ConnectorQuery where
  fmap f ma = ConnectorQuery $ \p0 p1 -> fmap f $ getConnectorQuery ma p0 p1



-- Applicative

instance Applicative ConnectorImage where
  pure a    = ConnectorImage $ \_  _  -> pure a
  mf <*> ma = ConnectorImage $ \p0 p1 -> 
                getConnectorImage mf p0 p1 <*> getConnectorImage ma p0 p1

instance Applicative ConnectorQuery where
  pure a    = ConnectorQuery $ \_  _  -> pure a
  mf <*> ma = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery mf p0 p1 <*> getConnectorQuery ma p0 p1


-- Monad 

instance Monad ConnectorImage where
  return a  = ConnectorImage $ \_  _  -> return a
  ma >>= k  = ConnectorImage $ \p0 p1 -> 
                getConnectorImage ma p0 p1 >>= \ans -> 
                getConnectorImage (k ans) p0 p1


instance Monad ConnectorQuery where
  return a  = ConnectorQuery $ \_  _  -> return a
  ma >>= k  = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery ma p0 p1 >>= \ans -> 
                getConnectorQuery (k ans) p0 p1


-- Monoid

instance Monoid a => Monoid (ConnectorImage a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorImage $ \p0 p1 -> 
                      getConnectorImage ma p0 p1 
                        `mappend` getConnectorImage mb p0 p1 


instance Monoid a => Monoid (ConnectorQuery a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorQuery $ \p0 p1 -> 
                      getConnectorQuery ma p0 p1 
                        `mappend` getConnectorQuery mb p0 p1 



-- ContextM

instance ContextM ConnectorImage where
  askCtx          = ConnectorImage $ \_  _  -> askCtx
  asksCtx fn      = ConnectorImage $ \_  _  -> asksCtx fn
  localize upd ma = ConnectorImage $ \p0 p1 -> 
                      localize upd (getConnectorImage ma p0 p1)

instance ContextM ConnectorQuery where
  askCtx          = ConnectorQuery $ \_  _  -> askCtx
  asksCtx fn      = ConnectorQuery $ \_  _  -> asksCtx fn
  localize upd ma = ConnectorQuery $ \p0 p1 -> 
                      localize upd (getConnectorQuery ma p0 p1)



runConnectorImage :: PdContext -> GenSt -> Port -> Port
                  -> ConnectorImage a
                  -> PrimResult a
runConnectorImage ctx st p0 p1 ma = 
    runImage ctx st $ getConnectorImage ma p0 p1


runConnectorQuery :: PdContext -> Port -> Port -> ConnectorQuery a -> a
runConnectorQuery ctx p0 p1 ma = 
    runQuery ctx $ getConnectorQuery ma p0 p1


connector :: ConnectorImage a -> Port -> Port -> Image a
connector ma p0 p1 = getConnectorImage ma p0 p1


promoteConn :: (Port -> Port -> Image a) -> ConnectorImage a
promoteConn k = ConnectorImage $ \p0 p1 -> k p0 p1

--------------------------------------------------------------------------------
-- Anchors

class HasID t where
  getID :: t -> Int



class HasID  t      => HasIn0 t
class HasIn0 t      => HasIn1 t
class HasIn1 t      => HasIn2 t

class HasID   t     => HasOut0 t
class HasOut0 t     => HasOut1 t
class HasOut1 t     => HasOut2 t



inport0 :: HasIn0 t => t -> Port
inport0 t = Port { parent_obj = getID t, port_num = 0 }

inport1 :: HasIn1 t => t -> Port
inport1 t = Port { parent_obj = getID t, port_num = 1 }

inport2 :: HasIn2 t => t -> Port
inport2 t = Port { parent_obj = getID t, port_num = 2 }

outport0 :: HasOut0 t => t -> Port
outport0 t = Port { parent_obj = getID t, port_num = 0 }

outport1 :: HasOut1 t => t -> Port
outport1 t = Port { parent_obj = getID t, port_num = 1 }

outport2 :: HasOut2 t => t -> Port
outport2 t = Port { parent_obj = getID t, port_num = 2 }
