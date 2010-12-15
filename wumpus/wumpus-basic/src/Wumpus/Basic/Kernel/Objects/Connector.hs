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

--  , intoConnectorImage


  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.BaseObjects

import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------
-- Connector Graphic


-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorCF u (GraphicAns u)


type DConnectorGraphic          = ConnectorGraphic Double




--------------------------------------------------------------------------------
-- Connector Image


-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path so
-- the Points ar @midway@, @atstart@ etc. can be taken on it.
--
type ConnectorImage u a = ConnectorCF u (ImageAns u a)


type DConnectorImage a  = ConnectorImage Double a




-- intoConnectorImage :: ConnectorCF u a 
--                    -> ConnectorGraphic u 
--                    -> ConnectorImage u a
-- intoConnectorImage = postcomb2 (,)




