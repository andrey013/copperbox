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
--   , uconvertConnectorImg

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
type ConnectorImage u a = ConnectorQuery u (ImageAns u a)


-- | ConnectorGraphic - function from DrawingContext and start and 
-- end points to a graphic /primitive/.
--
type ConnectorGraphic u = ConnectorQuery u (GraphicAns u)


-- | Type specialized version of 'ConnectorImage'.
--
type DConnectorImage a   = ConnectorImage Double a

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
intoConnectorImage :: ConnectorQuery u a
                   -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage qf ma = 
    promoteR2 $ \a b -> replaceAns <$> apply2R2 qf a b <*> apply2R2 ma a b  



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

{-
-- | Use this to convert both 'ConnectorImage' and 
-- 'ConnectorGraphic'.
--
uconvertConnectorImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                     => ConnectorImage t u -> ConnectorImage t u1
uconvertConnectorImg = uconvertR2ab

-}