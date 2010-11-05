{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.GraphicTypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Refined instances of of the Drawing type modelling specific
-- graphic types.
-- 
-- \*\* WARNING \*\* - some names are expected to change.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.GraphicTypes
  (


  -- * Function from Point to Point
    PointDisplace

  -- * Graphic  
  , Graphic
  , LocGraphic
  , LocThetaGraphic
  , ConnectorGraphic

  , DGraphic
  , DLocGraphic
  , DLocThetaGraphic
  , DConnectorGraphic


  -- * Image
  , Image
  , LocImage
  , LocThetaImage
  , ConnectorImage

  , DImage
  , DLocImage
  , DLocThetaImage
  , DConnectorImage

  -- * Iterated graphic
  , IterGraphic
  , DIterGraphic

  -- * Run functions
  , runGraphic
  , runLocGraphic
  , runImage
  , runLocImage

  -- * Combinators
  , moveLoc
  , at

  -- * Dropping answers
  , extrGraphic
  , extrLocGraphic


  , fontDeltaGraphic
  , fontDeltaImage
  , xlinkGraphic
  , xlinkImage

  , intoImage
  , intoLocImage
  , intoConnectorImage
  , intoLocThetaImage
  , makeIterGraphic


  ) where

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.Drawing
import Wumpus.Basic.Graphic.DrawingContext

import Wumpus.Core                      -- package: wumpus-core



type PointDisplace u = Point2 u -> Point2 u




--------------------------------------------------------------------------------
-- Graphic

-- Simple drawing - produce a primitive, access the DrawingContext
-- if required.
--
type Graphic u      = Drawing (PrimGraphic u)

-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u   = LocDrawing u (PrimGraphic u)


-- | A function from @point * angle -> graphic@
--
type LocThetaGraphic u          = LocThetaDrawing u (PrimGraphic u)

-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorDrawing u (PrimGraphic u)




type DGraphic           = Graphic Double
type DLocGraphic        = LocGraphic Double
type DLocThetaGraphic   = LocThetaGraphic Double
type DConnectorGraphic  = ConnectorGraphic Double


type instance DUnit (Graphic u) = u


--------------------------------------------------------------------------------
-- Image

-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a          = Drawing (a, PrimGraphic u)

type LocImage u a       = LocDrawing u (a,PrimGraphic u)

type LocThetaImage u a  = LocThetaDrawing u (a,PrimGraphic u)

-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path so
-- the Points ar @midway@, @atstart@ etc. can be taken on it.
--
type ConnectorImage u a = ConnectorDrawing u (a, PrimGraphic u)





type DImage a           = Image Double a
type DLocImage a        = LocImage Double a
type DLocThetaImage a   = LocThetaImage Double a 
type DConnectorImage a  = ConnectorImage Double a



type instance DUnit (Image u a) = u


--------------------------------------------------------------------------------


-- | Iterated Graphic - this partially models the @show@ command 
-- which draws the text and moves the /current point/ to the end 
-- of the text ready for the next words to be drawn.
--
type IterGraphic u      = LocImage u (Point2 u)

type DIterGraphic       = IterGraphic Double


--------------------------------------------------------------------------------
-- Graphic instances


-- Affine instances

instance (Real u, Floating u) => Rotate (Graphic u) where
  rotate ang = postpro (rotate ang) 


instance (Real u, Floating u) => RotateAbout (Graphic u) where
  rotateAbout ang pt = postpro (rotateAbout ang pt)


instance Num u => Scale (Graphic u) where
  scale sx sy = postpro (scale sx sy)


instance Num u => Translate (Graphic u) where
  translate dx dy = postpro (translate dx dy)

--------------------------------------------------------------------------------
-- Image instances

-- Affine instances


instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (Image u a) where
  rotate ang = postpro (\(a,b) -> (rotate ang a, rotate ang b))


instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = 
      postpro (\(a,b) -> (rotateAbout ang pt a, rotateAbout ang pt b))


instance (Num u, Scale a, DUnit a ~ u) => Scale (Image u a) where
  scale sx sy = postpro (\(a,b) -> (scale sx sy a, scale sx sy b))


instance (Num u, Translate a, DUnit a ~ u) => Translate (Image u a) where
  translate dx dy = postpro (\(a,b) -> (translate dx dy a, translate dx dy b))



--------------------------------------------------------------------------------
-- Run functions



runGraphic :: DrawingContext -> Graphic u -> PrimGraphic u
runGraphic ctx df = runDrawing ctx df


runLocGraphic :: DrawingContext -> LocGraphic u -> Point2 u -> PrimGraphic u
runLocGraphic ctx df = runDrawing ctx df



runImage :: DrawingContext -> Image u a -> (a, PrimGraphic u)
runImage ctx img = runDrawing ctx img

runLocImage :: DrawingContext -> LocImage u a -> Point2 u -> (a, PrimGraphic u)
runLocImage ctx img = runDrawing ctx img


--------------------------------------------------------------------------------
-- Combinators

moveLoc :: (Point2 u -> Point2 u) -> LocDrawing u a -> LocDrawing u a
moveLoc = prepro1


-- move to another module...

infixr 1 `at`
at :: Drawing (Point2 u -> b) -> Point2 u -> Drawing b
at = situ1




-------------------------------------------------------------------------------
-- Dropping /answers/


extrGraphic :: Image u a -> Graphic u
extrGraphic = postpro snd


extrLocGraphic :: LocImage u a -> LocGraphic u
extrLocGraphic = postpro1 snd 



--------------------------------------------------------------------------------


metamorphPrim :: (Primitive u -> Primitive u) -> PrimGraphic u -> PrimGraphic u
metamorphPrim f = primGraphic . f . getPrimGraphic

fontDeltaGraphic :: Graphic u -> Graphic u
fontDeltaGraphic df = 
    drawingCtx `bind` \ctx -> postpro (fun $ font_props ctx) df
  where 
    fun attr = metamorphPrim (fontDeltaContext attr)

fontDeltaImage :: Image u a -> Image u a
fontDeltaImage df = 
    drawingCtx `bind` \ctx -> postpro (fun $ font_props ctx) df
  where 
    fun attr = \(a,prim) -> (a, metamorphPrim (fontDeltaContext attr) prim)


xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic hypl = postpro (metamorphPrim (xlink hypl))


xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage hypl = 
    postpro (\(a,prim) -> (a, metamorphPrim (xlink hypl) prim))





intoImage :: Drawing a -> Graphic u -> Image u a
intoImage = postcomb (,)



intoLocImage :: LocDrawing u a -> LocGraphic u -> LocImage u a
intoLocImage = postcomb1 (,)

--    Drawing $ \ctx a -> (getDrawing f ctx a, getDrawing g ctx a)


intoConnectorImage :: ConnectorDrawing u a 
                   -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage = postcomb2 (,)



intoLocThetaImage :: LocThetaDrawing u a 
                  -> LocThetaGraphic u 
                  -> LocThetaImage u a
intoLocThetaImage = postcomb2 (,)


-- | Construction is different to intoZZ functions hence the 
-- different name.
--
makeIterGraphic :: PointDisplace u
                -> LocGraphic u 
                -> IterGraphic u
makeIterGraphic pf df = postcomb1 (,) (postpro1 pf locPoint) df





