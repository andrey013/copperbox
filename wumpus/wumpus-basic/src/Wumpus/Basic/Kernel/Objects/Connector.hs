{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
     ConnectorGraphic
   , ConnectorImage

   , connect

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.ImageBasis

import Wumpus.Core                              -- package: wumpus-core




-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype ConnectorImage t u = ConnectorImage { 
    getConnectorImage :: DrawingContext -> Point2 u -> Point2 u -> (t u, Primitive) }


-- | ConnectorGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type ConnectorGraphic u = ConnectorImage UNil u


instance OPlus (t u) => OPlus (ConnectorImage t u) where
  fa `oplus` fb = ConnectorImage $ \ctx p0 p1 -> 
                    getConnectorImage fa ctx p0 p1 `oplus` getConnectorImage fb ctx p0 p1

-- ConnectorImage is not a Functor - functor access would need to change 
-- the unit of the start point.

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapConnectorImage :: (t u -> t' u) -> (Primitive -> Primitive) 
           -> ConnectorImage t u -> ConnectorImage t' u
bimapConnectorImage l r gf = ConnectorImage $ \ctx p0 p1 -> 
    bimap l r $ getConnectorImage gf ctx p0 p1


-- This needs drawing context so cannot be done with 'bimapConnectorImage'.
--
instance UnitConvertExt t => UnitConvert (ConnectorImage t) where
  uconvert gf = ConnectorImage $ \ctx p0 p1 -> 
      let sz    = dc_font_size ctx
          (a,o) = getConnectorImage gf ctx (uconvertExt sz p0) (uconvertExt sz p1)
      in (uconvertExt sz a, o)

-- movestartConnectorImage :: (Point2 u -> Point2 u) 
--                         -> ConnectorImage t u -> ConnectorImage t u
-- movestartConnectorImage fn gf = 
--     ConnectorImage $ \ctx p0 p1 -> getConnectorImage gf ctx (fn p0 p1) 


-- Affine instances transfom the start point as well as the 
-- answer.

instance (InterpretUnit u, Rotate (t u)) => Rotate (ConnectorImage t u) where
  rotate ang gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (rotate ang)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (rotate ang a, rotate ang o)


instance (InterpretUnit u, Scale (t u)) => Scale (ConnectorImage t u) where
  scale sx sy gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (scale sx sy)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (scale sx sy a, scale sx sy o)


instance (InterpretUnit u, RotateAbout (t u)) => 
    RotateAbout (ConnectorImage t u) where
  rotateAbout ang pt gf = ConnectorImage $ \ctx p0 p1 -> 
      let dP0   = uconvertExt (dc_font_size ctx) pt
          trafo = intraMapPoint (dc_font_size ctx) (rotateAbout ang dP0)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (rotateAbout ang dP0 a, rotateAbout ang dP0 o)


instance (InterpretUnit u, Translate (t u)) => Translate (ConnectorImage t u) where
  translate dx dy gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (translate dx dy)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (translate dx dy a, translate dx dy o)


instance Localize ConnectorImage where
   localize upd gf = ConnectorImage $ \ctx p0 p1 -> 
                       getConnectorImage gf (upd ctx) p0 p1


instance Hyperlink (ConnectorImage t u) where
  hyperlink hyp = bimapConnectorImage id (xlinkPrim hyp)



instance IgnoreAns ConnectorImage where
  ignoreAns    = bimapConnectorImage (const UNil) id
  replaceAns o = bimapConnectorImage (const o) id


instance Annotate ConnectorImage where
  annotate = annoConnectorImg
  decorate = decoConnectorImg



decoConnectorImg :: ConnectorImage t u -> ConnectorGraphic u 
                 -> ConnectorImage t u
decoConnectorImg fa fb = ConnectorImage $ \ctx p0 p1 -> 
    let (a,o1) = getConnectorImage fa ctx p0 p1
        (_,o2) = getConnectorImage fb ctx p0 p1
    in (a, o1 `oplus` o2)
                        
annoConnectorImg :: ConnectorImage t u 
                 -> (t u -> ConnectorGraphic u) 
                 -> ConnectorImage t u
annoConnectorImg fa mf = ConnectorImage $ \ctx p0 p1 -> 
    let (a,o1) = getConnectorImage fa ctx p0 p1
        (_,o2) = getConnectorImage (mf a) ctx p0 p1
    in (a, o1 `oplus` o2)


connect :: ConnectorImage t u -> Point2 u -> Point2 u -> Image t u
connect gf p0 p1 = makeImageCtx $ \ctx -> getConnectorImage gf ctx p0 p1