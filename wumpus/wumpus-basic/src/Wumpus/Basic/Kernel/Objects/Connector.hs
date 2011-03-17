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

   , intoConnectorImage
   , emptyConnectorGraphic

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage


import Control.Applicative


-- | ConnectorImage - function from DrawingContext and start and 
-- end points to a polymorphic /answer/ and a graphic /primitive/.
--
type ConnectorImage t u = ConnectorQuery u (ImageAns t u)


-- | ConnectorGraphic - function from DrawingContext and start and 
-- end points to a graphic /primitive/.
--
type ConnectorGraphic u = ConnectorImage UNil u


-- | Type specialized version of 'ConnectorImage'.
--
type DConnectorImage t   = ConnectorImage t Double

-- | Type specialized version of 'ConnectorGraphic'.
--
type DConnectorGraphic   = ConnectorGraphic Double 



-- | 'intoConnectorImage' : @ conn_query * conn_graphic -> LocImage @
--
-- /Connector/ version of 'intoImage'. 
-- 
-- The 'ConnectorImage' is built as a function from an implicit 
-- start and end points to the answer.
--
intoConnectorImage :: ConnectorQuery u (t u) 
                   -> ConnectorGraphic u 
                   -> ConnectorImage t u
intoConnectorImage = liftA2 (\a (Ans _ p) -> Ans a p)


-- | 'emptyConnectorGraphic' : @ ConnectorGraphic @
--
-- Build an empty 'ConnectorGraphic'.
-- 
-- The 'emptyConnectorGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- bounding box around the rectangular hull of the start and end 
-- points.
-- 
emptyConnectorGraphic :: InterpretUnit u => ConnectorGraphic u 
emptyConnectorGraphic = promoteR2 $ \start end -> 
    let a = emptyLocGraphic `at` start
        b = emptyLocGraphic `at` end
    in a `oplus` b