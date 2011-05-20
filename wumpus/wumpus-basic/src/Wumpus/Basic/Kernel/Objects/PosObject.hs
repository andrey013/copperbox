{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosObject
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - a rectangular /positionable/ Image.
-- 
-- This graphic object has a more flexible API for positioning 
-- than other graphic objects. Rather than a LocGraphic which 
-- supports a single method of positioning at some start-point,
-- a @PosGraphic@ can be drawn at its center or locations on its 
-- outer rectangle.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosObject
  (

  -- * Positionable image

    PosObject
  , DPosObject


  -- * Operations
  , runPosObject

  , makePosObject
--  , makeBindPosObject
  , emptyPosObject


  , localPosObject
  , decoPosObject
  , extendPosObject
  , mapOrientation

  , illustratePosObject


  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Concat
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Orientation

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid


-- | Helper for PosObject - a LocImage that is /pre-applied/ to 
-- the DrawingContext.
--
-- This is somewhat contrived, but the orientation and the result
-- graphic from a PosImage have to be generated within the same 
-- DrawingContext.
--
type PosDraw u = Point2 u -> CatPrim


-- | A positionable \"Object\" that is drawn as a 
-- 'BoundedLocGraphic'.
--
newtype PosObject u = PosObject 
          { getPosObject :: Query u (Orientation u, PosDraw u) }

type instance DUnit (PosObject u) = u
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject = PosObject Double





--------------------------------------------------------------------------------


-- Monoid
instance (Fractional u, Ord u, InterpretUnit u) => Monoid (PosObject u) where
  mempty  = emptyPosObject
  mappend = poconcat



poconcat :: (Fractional u, Ord u) => PosObject u -> PosObject u -> PosObject u
poconcat a b = PosObject body
   where
    body = askDC >>= \ctx ->
           let ans1 = runQuery ctx (getPosObject a)
               ans2 = runQuery ctx (getPosObject b)
           in pure (appendW ans1 ans2)



appendW :: (Fractional u, Ord u)
        => (Orientation u, PosDraw u) 
        -> (Orientation u, PosDraw u) 
        -> (Orientation u, PosDraw u)
appendW (o0,pf0) (o1,pf1) = let pf = \pt -> pf0 pt `mappend` pf1 pt
                            in (o0 `mappend` o1, pf)



-- | Version of 'runPosObject' that produces a 
-- 'LocImage' that returns a bounding box. 
-- 
-- The 'PosObject' is run with only rect-address as an explicit 
-- argument (start-point is implicit). The corresponding answer is 
-- an /arity one/ Graphic that needs drawing with the start-point.
--
runPosObject :: (Fractional u, InterpretUnit u) 
             => RectAddress -> PosObject u -> LocImage u (BoundingBox u)
runPosObject addr (PosObject mf) = promoteLoc $ \pt ->
   askDC >>= \ctx -> 
   let (o1,df) = runQuery ctx mf
       v1      = orientationStart addr o1
       p1      = pt .+^ v1
       bb      = orientationBounds o1 p1
   in replaceAns bb $ primGraphic (df p1)



-- | 'makePosObject' : @ object_pos * loc_image -> PosObject @ 
--
-- Create a 'PosObject' from an 'Orientation' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
-- This is the /primary/ constructor for PosObjects. Because the
-- PosObject type is considered as a specialized object it does
-- not have the range of functions of LocImage or LocThetaImage.
-- 
makePosObject :: InterpretUnit u
              => Query u (Orientation u) -> LocGraphic u -> PosObject u
makePosObject qortt gf = PosObject body
  where
    body = askDC >>= \ctx -> 
           let v1   = runQuery ctx qortt
               pf   = \pt -> getCP $ runLocImage pt ctx gf
           in return (v1,pf)

    getCP (PrimW ca _) = ca


-- | 'emptyPosObject' : @ PosObject @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosObject :: InterpretUnit u => PosObject u
emptyPosObject = PosObject $ pure (Orientation 0 0 0 0, const mempty)

    



-- | Apply a DrawingContext update to a 'PosObject'.
--
localPosObject :: DrawingContextF -> PosObject u -> PosObject u
localPosObject upd = PosObject . localize upd . getPosObject


decoPosObject :: InterpretUnit u 
              => (Orientation u -> LocGraphic u) 
              -> ZDeco -> PosObject u -> PosObject u
decoPosObject fn zdec po = PosObject body
  where
    body = askDC >>= \ctx -> 
           let (ortt,ptf) = runQuery ctx (getPosObject po)
               deco       = \pt -> getCP $ runLocImage pt ctx (fn ortt)
               gf         = case zdec of
                              ANTERIOR -> deco `mappend` ptf
                              SUPERIOR -> ptf  `mappend` deco
           in return (ortt, gf)

    getCP (PrimW ca _) = ca



-- | Extend the orientation.
--
extendPosObject :: Num u 
                => u -> u -> u -> u -> PosObject u -> PosObject u
extendPosObject x0 x1 y0 y1 po = PosObject body
  where
    body = askDC >>= \ctx -> 
           let (o0,pf0) = runQuery ctx (getPosObject po)
               ortt     = extendOrientation x0 x1 y0 y1 o0
           in return (ortt,pf0)



mapOrientation :: (Orientation u -> Orientation u) -> PosObject u -> PosObject u
mapOrientation fn po = PosObject body
  where
    body = askDC >>= \ctx -> 
           let (o0,pf0) = runQuery ctx (getPosObject po)
           in return (fn o0,pf0)


--------------------------------------------------------------------------------


-- | Illustrate a 'PosObject' by super-imposing its 'Orientation'.
--
-- This turns the 'PosObject' into a 'LocImage' drawn at the locus
-- of the PosObject.
--
illustratePosObject :: InterpretUnit u 
                   => PosObject u -> LocGraphic u
illustratePosObject (PosObject mf)  = promoteLoc $ \pt ->   
    zapQuery mf >>= \(ortt,ptf) -> 
    adecorate (primGraphic $ ptf pt) (illustrateOrientation ortt `at` pt)


illustrateOrientation :: InterpretUnit u 
                    => Orientation u -> LocGraphic u
illustrateOrientation (Orientation xmin xmaj ymin ymaj) = promoteLoc $ \pt -> 
    dinterpCtx 3 >>= \radius -> 
    let upd = localize (fill_colour blue . dotted_line)
        bl  = pt .-^ V2 xmin ymin
        dot = localize (fill_colour red) $ dcDisk FILL radius `at` pt
        hln = upd $ locStraightLine (hvec $ xmin+xmaj) `at` pt .-^ hvec xmin
        vln = upd $ locStraightLine (vvec $ ymin+ymaj) `at` pt .-^ vvec ymin
        bdr = upd $ dcRectangle STROKE (xmin+xmaj) (ymin+ymaj) `at` bl
    in mconcat [ bdr, hln, vln, dot ]


--------------------------------------------------------------------------------
-- Combining PosObject


instance (Fractional u, Ord u, InterpretUnit u) => ZConcat (PosObject u) where
  superior = mappend
  anterior = flip mappend


instance (Num u, Ord u) => Concat (PosObject u) where
  hconcat = genMoveAlign spinemoveH spineRight
  vconcat = genMoveAlign spinemoveV spineBelow

instance (Num u, Ord u) => CatSpace (PosObject u) where
  hspace = genMoveSepH spinemoveH spineRight
  vspace = genMoveSepV spinemoveV spineBelow



instance (Fractional u, Ord u) => Align (PosObject u) where
  halign HALIGN_TOP    = genMoveAlign binmoveHTop    halignTopO
  halign HALIGN_CENTER = genMoveAlign binmoveHCenter halignCenterO
  halign HALIGN_BASE   = genMoveAlign binmoveHBottom halignBottomO

  valign VALIGN_LEFT   = genMoveAlign binmoveVLeft   valignLeftO
  valign VALIGN_CENTER = genMoveAlign binmoveVCenter valignCenterO
  valign VALIGN_RIGHT  = genMoveAlign binmoveVRight  valignRightO



genMoveAlign :: (Num u)   
             => (Orientation u -> Orientation u -> Vec2 u) 
             -> (Orientation u -> Orientation u -> Orientation u) 
             -> PosObject u -> PosObject u -> PosObject u
genMoveAlign mkV mkO po0 po1 = PosObject body
  where
   body = askDC >>= \ctx -> 
          let (ortt0,pf0) = runQuery ctx (getPosObject po0)
              (ortt1,pf1) = runQuery ctx (getPosObject po1)
              v1          = mkV ortt0 ortt1
              ortt        = mkO ortt0 ortt1
              pf          = \pt -> pf0 pt `mappend` (pf1 $ pt .+^ v1)
          in return (ortt,pf)


--------------------------------------------------------------------------------
-- Sep

instance (Fractional u, Ord u) => AlignSpace (PosObject u) where
  halignSpace HALIGN_TOP    = genMoveSepH binmoveHTop    halignTopO
  halignSpace HALIGN_CENTER = genMoveSepH binmoveHCenter halignCenterO
  halignSpace HALIGN_BASE   = genMoveSepH binmoveHBottom halignBottomO

  valignSpace VALIGN_LEFT   = genMoveSepV binmoveVLeft   valignLeftO
  valignSpace VALIGN_CENTER = genMoveSepV binmoveVCenter valignCenterO
  valignSpace VALIGN_RIGHT  = genMoveSepV binmoveVRight  valignRightO


genMoveSepH :: (Num u)   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject u -> PosObject u -> PosObject u
genMoveSepH mkV mkO sep po0 po1  = PosObject body
  where
    body = askDC >>= \ctx -> 
           let (ortt0,pf0) = runQuery ctx (getPosObject po0)
               (ortt1,pf1) = runQuery ctx (getPosObject po1)
               v1          = hvec sep ^+^ mkV ortt0 ortt1
               ortt        = extendORight sep $ mkO ortt0 ortt1
               pf          = \pt -> pf0 pt `mappend` (pf1 $ pt .+^ v1)
           in return (ortt,pf)


genMoveSepV :: (Num u)   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject u -> PosObject u -> PosObject u
genMoveSepV mkV mkO sep po0 po1 = PosObject body
  where
    body = askDC >>= \ctx -> 
           let (ortt0,pf0) = runQuery ctx (getPosObject po0)
               (ortt1,pf1) = runQuery ctx (getPosObject po1)
               v1          = vvec (-sep) ^+^ mkV ortt0 ortt1
               ortt        = extendODown sep $ mkO ortt0 ortt1
               pf          = \pt -> pf0 pt `mappend` (pf1 $ pt .+^ v1)
           in return (ortt,pf)

