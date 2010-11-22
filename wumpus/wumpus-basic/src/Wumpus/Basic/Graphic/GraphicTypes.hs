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

  -- * Advance vector
  , AdvanceVec

  -- * Drawing info
  , DrawingInfo
  , LocDrawingInfo
  , LocThetaDrawingInfo

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

  -- * /Advance vector/ graphic
  , AdvGraphic
  , DAdvGraphic

  -- * Bounded graphic / loc graphic
  , BoundedGraphic
  , DBoundedGraphic
  , BoundedLocGraphic
  , DBoundedLocGraphic

  -- * Extract from an Advance vector
  , advanceH
  , advanceV

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
  , makeAdvGraphic


  ) where

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.ContextFunction
import Wumpus.Basic.Graphic.DrawingContext

import Wumpus.Core                      -- package: wumpus-core



type PointDisplace u = Point2 u -> Point2 u


type AdvanceVec u = Vec2 u






--------------------------------------------------------------------------------
-- DrawingInfo

-- | A query on the DrawingContext.
--
-- Alias for 'CF'.
-- 
type DrawingInfo a      = CF a


-- | A query on the DrawingContext respective to the supplied
--  point.
--
-- Alias for 'LocCF'.
-- 
type LocDrawingInfo u a   = LocCF u a


-- | A query on the DrawingContext respective to the supplied
--  point and angle.
--
-- Alias for 'LocCF'.
-- 
type LocThetaDrawingInfo u a   = LocThetaCF u a


--------------------------------------------------------------------------------
-- Graphic

-- | Simple drawing - produce a primitive, access the DrawingContext
-- if required.
--
type Graphic u      = CF (PrimGraphic u)

-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u   = LocCF u (PrimGraphic u)


-- | A function from @point * angle -> graphic@
--
type LocThetaGraphic u          = LocThetaCF u (PrimGraphic u)

-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorCF u (PrimGraphic u)




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
type Image u a          = CF (a, PrimGraphic u)

type LocImage u a       = LocCF u (a,PrimGraphic u)

type LocThetaImage u a  = LocThetaCF u (a,PrimGraphic u)

-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path so
-- the Points ar @midway@, @atstart@ etc. can be taken on it.
--
type ConnectorImage u a = ConnectorCF u (a, PrimGraphic u)





type DImage a           = Image Double a
type DLocImage a        = LocImage Double a
type DLocThetaImage a   = LocThetaImage Double a 
type DConnectorImage a  = ConnectorImage Double a



type instance DUnit (Image u a) = u


--------------------------------------------------------------------------------


-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- width (advance) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Point2 u)

type DAdvGraphic       = AdvGraphic Double


type instance DUnit (AdvGraphic u) = u


--------------------------------------------------------------------------------

-- | Graphic with a bounding box.
-- 
type BoundedGraphic u      = Image u (BoundingBox u)

type DBoundedGraphic       = BoundedGraphic Double


type instance DUnit (BoundedGraphic u) = u


-- | LocGraphic with a bounding box.
--
type BoundedLocGraphic u      = LocImage u (BoundingBox u)

type DBoundedLocGraphic       = BoundedLocGraphic Double


type instance DUnit (BoundedLocGraphic u) = u



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

-- | Extract the horizontal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0. Ingoring it seems 
-- permissible, e.g. when calculating bounding boxes for 
-- left-to-right text.
--
advanceH :: Num u => AdvanceVec u -> u
advanceH (V2 w _)  = w

-- | Extract the verticaltal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0.
--
advanceV :: Num u => AdvanceVec u -> u
advanceV (V2 _ h)  = h


--------------------------------------------------------------------------------
-- Run functions



runGraphic :: DrawingContext -> Graphic u -> PrimGraphic u
runGraphic ctx df = runCF ctx df


runLocGraphic :: DrawingContext -> Point2 u -> LocGraphic u -> PrimGraphic u
runLocGraphic ctx pt df = runCF ctx (unCF1 pt df)



runImage :: DrawingContext -> Image u a -> (a, PrimGraphic u)
runImage ctx img = runCF ctx img

runLocImage :: DrawingContext -> Point2 u -> LocImage u a -> (a, PrimGraphic u)
runLocImage ctx pt img = runCF ctx (unCF1 pt img)


--------------------------------------------------------------------------------
-- Combinators

moveLoc :: (Point2 u -> Point2 u) -> LocCF u a -> LocCF u a
moveLoc = prepro1



infixr 1 `at`
at :: CF (Point2 u -> b) -> Point2 u -> CF b
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





intoImage :: CF a -> Graphic u -> Image u a
intoImage = postcomb (,)



intoLocImage :: LocCF u a -> LocGraphic u -> LocImage u a
intoLocImage = postcomb1 (,)

--    Drawing $ \ctx a -> (getDrawing f ctx a, getDrawing g ctx a)


intoConnectorImage :: ConnectorCF u a 
                   -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage = postcomb2 (,)



intoLocThetaImage :: LocThetaCF u a 
                  -> LocThetaGraphic u 
                  -> LocThetaImage u a
intoLocThetaImage = postcomb2 (,)


-- | Construction is different to intoZZ functions hence the 
-- different name.
--
makeAdvGraphic :: PointDisplace u
               -> LocGraphic u 
               -> AdvGraphic u
makeAdvGraphic pf df = postcomb1 (,) (postpro1 pf locPoint) df





