{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- A Drawing object. 
-- 
-- This is the corresponding type to Picture in the Wumpus-Core.
-- 
-- Drawing is a function from the DrawingContext to a Picture.
-- Internally the result is actually a (Maybe Picture) and not a 
-- Picture, this is a trick to promote the extraction from 
-- possibly empty drawings (created by TraceDrawing) to the 
-- top-level of the type hierarchy where client code can deal 
-- with empty drawings explicitly (empty Pictures cannot be 
-- rendered by Wumpus-Core).
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Drawing
  (

    Drawing
  , DDrawing
  , runDrawing
  , runDrawingU
  , drawTracing

  , clipDrawing
  , modifyDrawing
  , drawingConcat

  ) where

import Wumpus.Basic.Graphic.ContextFunction
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.TraceDrawing

import Wumpus.Core                              -- package: wumpus-core

newtype Drawing u = Drawing { getDrawing :: CF (Maybe (Picture u)) }

type DDrawing = Drawing Double


type instance DUnit (Drawing u) = u




runDrawing :: DrawingContext -> Drawing u -> Maybe (Picture u)
runDrawing ctx drw = runCF ctx (getDrawing drw)  


runDrawingU :: DrawingContext -> Drawing u -> Picture u
runDrawingU ctx df = maybe fk id $ runDrawing ctx df
  where
    fk = error "runDrawingU - empty Drawing."   



drawTracing :: (Real u, Floating u, FromPtSize u) 
            => TraceDrawing u a -> Drawing u
drawTracing mf = Drawing $ 
    drawingCtx >>= \ctx -> return (liftToPictureMb (execTraceDrawing ctx mf) )


-- Note - cannot get an answer from a TraceDrawing with this 
-- Drawing type. There is nowhere to put the answer in the type.
--
-- If the type was extended:
--
-- > newtype Drawing u a = Drawing { getDrawing :: CF (a, Maybe (Picture u))) }
--
-- It would make things difficult for the drawing composition 
-- operators. @a@ could be monoidial but are there any types of 
-- a where this would be useful (rather than just making things 
-- more complicated)? 
--
--------------------------------------------------------------------------------

clipDrawing :: (Num u, Ord u) => (PrimPath u) -> Drawing u -> Drawing u
clipDrawing cpath = modifyDrawing (clip cpath)


modifyDrawing :: (Picture u -> Picture u) -> Drawing u -> Drawing u
modifyDrawing pf = Drawing . postpro (fmap pf) . getDrawing

instance (Real u, Floating u) => Rotate (Drawing u) where 
  rotate ang = modifyDrawing (rotate ang)

instance (Real u, Floating u) => RotateAbout (Drawing u) where
  rotateAbout r pt = modifyDrawing (rotateAbout r pt)

instance (Num u, Ord u) => Scale (Drawing u) where
  scale sx sy = modifyDrawing (scale sx sy)

instance (Num u, Ord u) => Translate (Drawing u) where
  translate dx dy = modifyDrawing (translate dx dy)





drawingConcat :: (Picture u -> Picture u -> Picture u) 
              -> Drawing u -> Drawing u -> Drawing u
drawingConcat op a b = Drawing $ mbpostcomb op (getDrawing a) (getDrawing b)



mbpostcomb :: (a -> a -> a) -> CF (Maybe a) -> CF (Maybe a) -> CF (Maybe a)
mbpostcomb op = postcomb fn
  where
    fn (Just a) (Just b) = Just $ a `op` b
    fn a        Nothing  = a
    fn Nothing  b        = b
