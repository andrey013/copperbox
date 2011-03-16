{-# LANGUAGE TypeFamilies               #-}
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
   , ConnectorQuery

   , runConnectorImage
   , rawConnectorImage
   , connect

   , intoConnectorImage
   , makeConnectorGraphic

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype ConnectorImage r u = ConnectorImage { 
    getConnectorImage :: DrawingContext -> 
                         Point2 u       -> Point2 u  -> (r u, CatPrim) }


type instance Answer (ConnectorImage r u)     = r u
type instance ArgDiff (ConnectorImage r u) (Image r u) = (Point2 u, Point2 u)


-- | ConnectorGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type ConnectorGraphic u = ConnectorImage UNil u


type ConnectorQuery u ans = Query (Point2 u -> Point2 u -> ans)


--------------------------------------------------------------------------------


instance PromoteR2 (ConnectorImage r u) (Image r u) where
  promoteR2 = promoteConnectorImg

instance Lift0R2 (ConnectorImage r u) (Image r u) where
  lift0R2 = liftConnectorImg


instance BindQuery (ConnectorImage r u) where
   (&=>) = bindQuery


instance BindQueryR2 (ConnectorImage r u) (Image r u) where
   (&===>) = bindR2



--------------------------------------------------------------------------------


instance OPlus (r u) => OPlus (ConnectorImage r u) where
  fa `oplus` fb = ConnectorImage $ \ctx p0 p1 -> 
      getConnectorImage fa ctx p0 p1 `oplus` getConnectorImage fb ctx p0 p1

-- ConnectorImage is not a Functor - functor access would need to change 
-- the unit of the start point.

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapConnectorImage :: (r u -> r1 u) -> (Primitive -> Primitive) 
           -> ConnectorImage r u -> ConnectorImage r1 u
bimapConnectorImage l r gf = ConnectorImage $ \ctx p0 p1 -> 
    bimap l (cpmap r) $ getConnectorImage gf ctx p0 p1


-- This needs drawing context so cannot be done with 'bimapConnectorImage'.
--
instance Functor t => UnitConvert (ConnectorImage t) where
  uconvert gf = ConnectorImage $ \ctx p0 p1 -> 
      let sz    = dc_font_size ctx
          (a,o) = getConnectorImage gf ctx (uconvertF sz p0) (uconvertF sz p1)
      in (uconvertF sz a, o)

-- movestartConnectorImage :: (Point2 u -> Point2 u) 
--                         -> ConnectorImage r u -> ConnectorImage r u
-- movestartConnectorImage fn gf = 
--     ConnectorImage $ \ctx p0 p1 -> getConnectorImage gf ctx (fn p0 p1) 


-- Affine instances transfom the start point as well as the 
-- answer.

instance (InterpretUnit u, Rotate (r u)) => Rotate (ConnectorImage r u) where
  rotate ang gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (rotate ang)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (rotate ang a, rotate ang o)


instance (InterpretUnit u, Scale (r u)) => Scale (ConnectorImage r u) where
  scale sx sy gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (scale sx sy)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (scale sx sy a, scale sx sy o)


instance (InterpretUnit u, RotateAbout (r u)) => 
    RotateAbout (ConnectorImage r u) where
  rotateAbout ang pt gf = ConnectorImage $ \ctx p0 p1 -> 
      let dP0   = uconvertF (dc_font_size ctx) pt
          trafo = intraMapPoint (dc_font_size ctx) (rotateAbout ang dP0)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (rotateAbout ang dP0 a, rotateAbout ang dP0 o)


instance (InterpretUnit u, Translate (r u)) => Translate (ConnectorImage r u) where
  translate dx dy gf = ConnectorImage $ \ctx p0 p1 -> 
      let trafo = intraMapPoint (dc_font_size ctx) (translate dx dy)
          (a,o) = getConnectorImage gf ctx (trafo p0) (trafo p1)
      in (translate dx dy a, translate dx dy o)





instance Object ConnectorImage where
  local_ctx     = localConnectorImg
  ignoreAns     = bimapConnectorImage (const UNil) id
  replaceAns o  = bimapConnectorImage (const o) id
  mapAns f      = bimapConnectorImage f id
  hyperlink hyp = bimapConnectorImage id (xlinkPrim hyp)
  clipObject pp = bimapConnectorImage id (clip pp)
  annotate      = annoConnectorImg
  decorate      = decoConnectorImg
  bind          = bindConnectorImg
  unit          = unitConnectorImg


localConnectorImg :: (DrawingContext -> DrawingContext) 
                  -> ConnectorImage r u 
                  -> ConnectorImage r u
localConnectorImg upd gf = ConnectorImage $ \ctx p0 p1 -> 
                           getConnectorImage gf (upd ctx) p0 p1




bindConnectorImg :: ConnectorImage r u -> (r u -> ConnectorImage r1 u) 
                 -> ConnectorImage r1 u
bindConnectorImg gf fn = ConnectorImage $ \ctx p0 p1 -> 
    let (a,o1) = getConnectorImage gf ctx p0 p1
        (b,o2) = getConnectorImage (fn a) ctx p0 p1
    in (b, o1 `oplus` o2)

unitConnectorImg :: r u -> ConnectorImage r u
unitConnectorImg a = ConnectorImage $ \_ _ _ -> (a,mempty)





decoConnectorImg :: ConnectorImage r u -> ConnectorGraphic u 
                 -> ConnectorImage r u
decoConnectorImg fa fb = ConnectorImage $ \ctx p0 p1 -> 
    let (a,o1) = getConnectorImage fa ctx p0 p1
        (_,o2) = getConnectorImage fb ctx p0 p1
    in (a, o1 `oplus` o2)
                        
annoConnectorImg :: ConnectorImage r u 
                 -> (r u -> ConnectorGraphic u) 
                 -> ConnectorImage r u
annoConnectorImg fa mf = ConnectorImage $ \ctx p0 p1 -> 
    let (a,o1) = getConnectorImage fa ctx p0 p1
        (_,o2) = getConnectorImage (mf a) ctx p0 p1
    in (a, o1 `oplus` o2)


--------------------------------------------------------------------------------
-- builders and destructors


runConnectorImage :: ConnectorImage r u 
                  -> DrawingContext 
                  -> Point2 u 
                  -> Point2 u 
                  -> (r u, CatPrim)
runConnectorImage gf ctx p0 p1 = getConnectorImage gf ctx p0 p1



-- This seems to be the one for down casting...
-- 
rawConnectorImage :: (DrawingContext -> Point2 u -> Point2 u -> (r u, CatPrim)) 
                  -> ConnectorImage r u
rawConnectorImage fn = ConnectorImage $ \ctx p0 p1 -> fn ctx p0 p1


-- | Downcast a 'ConnectorCF' function by applying it to the 
-- start and end point, making an arity-zero Context Function 
-- (a 'CF'). 
-- 
connect :: ConnectorImage r u -> Point2 u -> Point2 u -> Image r u
connect gf p0 p1 = rawImage $ \ctx -> getConnectorImage gf ctx p0 p1


intoConnectorImage :: ConnectorQuery u (r u) -> ConnectorGraphic u 
                   -> ConnectorImage r u
intoConnectorImage fn gf = ConnectorImage $ \ctx p0 p1 -> 
   let ans   = runQuery fn ctx p0 p1
       (_,o) = getConnectorImage gf ctx p0 p1
   in (ans,o)


makeConnectorGraphic :: InterpretUnit u 
                     => (DrawingContext -> a) 
                     -> (a -> DPoint2 -> DPoint2 -> Primitive) 
                     -> ConnectorGraphic u
makeConnectorGraphic qry fn = ConnectorImage $ \ctx p0 p1 -> 
    let a  = qry ctx 
        sz = dc_font_size ctx
    in (UNil, prim1 $ fn a (uconvertF sz p0) (uconvertF sz p1))



promoteConnectorImg :: (Point2 u -> Point2 u -> Image r u) -> ConnectorImage r u
promoteConnectorImg gf = 
    ConnectorImage $ \ctx p0 p1 -> runImage (gf p0 p1) ctx


liftConnectorImg :: Image r u -> ConnectorImage r u
liftConnectorImg gf = ConnectorImage $ \ctx _ _ -> runImage gf ctx 



bindQuery :: Query ans -> (ans -> ConnectorImage r u) -> ConnectorImage r u
bindQuery qy fn = ConnectorImage $ \ctx p0 p1 -> 
    let a = runQuery qy ctx in runConnectorImage (fn a) ctx p0 p1


bindR2 :: ConnectorQuery u ans -> (ans -> Image r u) -> ConnectorImage r u
bindR2 qy fn = ConnectorImage $ \ctx p0 p1 -> 
    let f1 = runQuery qy ctx in runImage (fn $ f1 p0 p1) ctx
