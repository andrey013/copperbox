{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Connector
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic and Image types representing connectors - connectors
-- have two implicit points - start and end.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Connector
  (

  -- * Connector Graphic
    ConnectorGraphic
  , DConnectorGraphic


  -- * Connector Image
  , ConnectorImage
  , DConnectorImage

  , intoConnectorImage
  , emptyConnectorGraphic

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

--------------------------------------------------------------------------------
-- Connector Graphic


-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorCF u (GraphicAns u)

-- | Alias of 'ConnectorGraphic' where the unit type is 
-- specialized to Double. 
--
type DConnectorGraphic          = ConnectorGraphic Double




--------------------------------------------------------------------------------
-- Connector Image


-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path 
-- (defined in Wumpus-Drawing) so the points at @midway@, 
-- @atstart@ etc. or the end directions and tangents can be taken 
-- on it.
--
type ConnectorImage u a = ConnectorCF u (ImageAns u a)


-- | Alias of 'ConnectorImage' where the unit type is 
-- specialized to Double. 
--
type DConnectorImage a  = ConnectorImage Double a



-- | 'intoConnectorImage' : @ conn_context_function * conn_graphic -> LocImage @
--
-- /Connector/ version of 'intoImage'. 
-- 
-- The 'ConnectorImage' is built as a function from an implicit 
-- start and end points to the answer.
--
intoConnectorImage :: ConnectorCF u a -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage = liftA2 (\a (_,b) -> (a,b))


-- | 'emptyConnectorGraphic' : @ ConnectorGraphic @
--
-- Build an empty 'ConnectorGraphic'.
-- 
-- The 'emptyConnectorGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- bounding box around the rectangular hull of the start and end 
-- points.
-- 
emptyConnectorGraphic :: PtSize u => ConnectorGraphic u 
emptyConnectorGraphic = promoteR2 $ \start end -> 
    let a = emptyLocGraphic `at` start
        b = emptyLocGraphic `at` end
    in a `oplus` b


