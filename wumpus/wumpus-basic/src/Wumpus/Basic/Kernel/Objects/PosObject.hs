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

  , LocRectQuery
  , BoundedLocRectGraphic

  -- * Operations

  , makePosObject
  , makeBindPosObject
  , emptyPosObject
  , runPosObject
  , localizePO 
  , elaboratePO
  , aelaboratePO 
 
  , makeBoundedLocRectGraphic
  , startAddr
  , atStartAddr

  , extendPosObject

  , padHorizontalPO
  , padLeftPO
  , padRightPO
  , padVerticalPO
  , padUpPO
  , padDownPO

  , illustratePosObject


  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Bounded
import Wumpus.Basic.Kernel.Objects.Concat
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Displacement
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
type PosDraw u = Point2 u -> GraphicAns u


-- | A positionable \"Object\" that is drawn as a 
-- 'BoundedLocGraphic'.
--
newtype PosObject u = PosObject 
          { getPosObject :: CF (Orientation u, PosDraw u) }

type instance DUnit (PosObject u) = u
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject = PosObject Double




type LocRectQuery u a = CF (Point2 u ->  RectAddress -> a)

type BoundedLocRectGraphic u = LocRectQuery u (ImageAns u (BoundingBox u))

--------------------------------------------------------------------------------


instance (Fractional u, Ord u) => OPlus (PosObject u) where
  oplus = poconcat

instance (Fractional u, Ord u, InterpretUnit u) => Monoid (PosObject u) where
  mempty = pozero
  mappend = poconcat


pozero :: InterpretUnit u => PosObject u
pozero = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let pf = \pt -> runCF ctx (apply1R1 emptyLocGraphic pt)
           in return (Orientation 0 0 0 0, pf)

poconcat :: (Fractional u, Ord u) => PosObject u -> PosObject u -> PosObject u
poconcat a b = PosObject body
   where
     body = drawingCtx >>= \ctx -> 
            let (o0,pf0) = runCF ctx (getPosObject a)
                (o1,pf1) = runCF ctx (getPosObject b)
                pf       = \pt -> pf0 pt `oplus` pf1 pt
            in return (o0 `oplus` o1, pf)


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
makePosObject :: Query (Orientation u) -> LocGraphic u -> PosObject u
makePosObject qortt gf = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let ortt = runCF ctx qortt
               pf   = runCF ctx gf
           in return (ortt,pf)


-- | This is a bit of a hack to overcome that the newtype 
-- wrapper around PosObject stops monadic bind operating 
-- with the internal CF function.
--
makeBindPosObject :: Query a 
                  -> (a -> Query (Orientation u)) -> (a -> LocGraphic u) 
                  -> PosObject u 
makeBindPosObject qy mkO mkG = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let a    = runCF ctx qy
               ortt = runCF ctx (mkO a)
               pf   = runCF ctx (mkG a)
           in return (ortt,pf)



-- | 'emptyPosObject' : @ PosObject @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosObject :: InterpretUnit u => PosObject u
emptyPosObject = 
    makePosObject (pure $ Orientation 0 0 0 0) emptyLocGraphic

    
-- | Run a PosObject forming an Image.
--
runPosObject :: Fractional u 
             => Point2 u -> RectAddress -> PosObject u -> BoundedGraphic u
runPosObject pt addr (PosObject mf) = 
    mf >>= \(ortt,ptf) -> let sv = orientationStart addr ortt
                              bb = orientationBounds ortt (displaceVec sv pt)
                          in pure $ replaceAns bb $ ptf $ displaceVec sv pt


-- | Run a DrawingContext update within a 'PosObject'.
--
localizePO :: DrawingContextF -> PosObject u -> PosObject u
localizePO upd = PosObject . localize upd . getPosObject


-- | 'decorate' -like functionality.
--
elaboratePO :: (Orientation u -> LocGraphic u) -> PosObject u -> PosObject u
elaboratePO fn po = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt,ptf) = runCF ctx (getPosObject po)
               deco       = runCF ctx (fn ortt)
           in return (ortt, ptf `oplus` deco)

-- | ante-eloborate
--
aelaboratePO :: (Orientation u -> LocGraphic u) -> PosObject u -> PosObject u
aelaboratePO fn po = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt,ptf) = runCF ctx (getPosObject po)
               deco       = runCF ctx (fn ortt)
           in return (ortt, deco `oplus` ptf)



-- | Make a 'BoundedLocRectGraphic' from a 'PosObject'.
-- 
-- This turns a PosObject (concatenatable) into a LocRectImage 
-- (drawable at rectangle positions).
--
makeBoundedLocRectGraphic :: Fractional u 
                          => PosObject u -> BoundedLocRectGraphic u
makeBoundedLocRectGraphic po = promoteR2 $ \pt addr -> runPosObject pt addr po



infixr 1 `startAddr`

-- | 'startAddr' : @ bounded_loc_rect * rect_pos -> BoundedlocGraphic @
--
-- /Downcast/ a 'BoundedLocRectGraphic' to a 'BoundedLocGraphic' 
-- by supplying it with a 'RectAddress' (start address on the 
-- rectangle frame).
--  
startAddr :: Floating u 
          => BoundedLocRectGraphic u -> RectAddress -> BoundedLocGraphic u
startAddr = apply1R2 



-- | 'atStartAddr' : @ bounded_loc_rect * start_point * rect_pos 
--      -> BoundedGraphic @
--
-- /Downcast/ a 'BoundedLocRectGraphic' to a 'BoundedGraphic' by 
-- supplying it with an initial point and a 'RectAddress' (start 
-- address on the rectangle frame).
--  
atStartAddr ::  Floating u 
            => BoundedLocRectGraphic u -> Point2 u -> RectAddress 
            -> BoundedGraphic u
atStartAddr = apply2R2



-- | Extend the orientation.
--
extendPosObject :: Num u 
                => u -> u -> u -> u -> PosObject u -> PosObject u
extendPosObject x0 x1 y0 y1 po = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (o0,pf0) = runCF ctx (getPosObject po)
               ortt     = extendOrientation x0 x1 y0 y1 o0
           in return (ortt,pf0)

           
--------------------------------------------------------------------------------
-- Padding

padHorizontalPO     :: (Fractional u, Ord u) => u -> PosObject u -> PosObject u
padHorizontalPO w   = genPad (padHEven w)

padLeftPO       :: (Num u, Ord u) => u -> PosObject u -> PosObject u
padLeftPO w     = genPad (padXMinor w)

padRightPO      :: (Num u, Ord u) => u -> PosObject u -> PosObject u
padRightPO w    = genPad (padXMajor w)


padVerticalPO       :: (Fractional u, Ord u) => u -> PosObject u -> PosObject u
padVerticalPO w     = genPad (padVEven w)

padUpPO         :: (Num u, Ord u) => u -> PosObject u -> PosObject u
padUpPO h       = genPad (padYMajor h)

padDownPO       :: (Num u, Ord u) => u -> PosObject u -> PosObject u
padDownPO h     = genPad (padYMinor h)


genPad :: (Orientation u -> Orientation u) -> PosObject u -> PosObject u
genPad fn po = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (o0,pf0) = runCF ctx (getPosObject po)
               ortt     = fn o0
           in return (ortt,pf0)


--------------------------------------------------------------------------------


-- | Illustrate a 'PosObject' by super-imposing its 'Orientation'.
--
-- This turns the 'PosObject' into a 'LocImage' drawn at the locus
-- of the PosObject.
--
illustratePosObject :: InterpretUnit u 
                   => PosObject u -> LocGraphic u
illustratePosObject (PosObject mf)  = promoteR1 $ \pt ->   
    mf >>= \(ortt,ptf) -> 
    decorateR0 (pure $ ptf pt) (illustrateOrientation ortt `at` pt)


illustrateOrientation :: InterpretUnit u 
                    => Orientation u -> LocGraphic u
illustrateOrientation (Orientation xmin xmaj ymin ymaj) = promoteR1 $ \pt -> 
    dinterpCtx 3 >>= \radius -> 
    let upd = localize (fill_colour blue . dotted_line)
        bl  = pt .-^ V2 xmin ymin
        dot = localize (fill_colour red) $ filledDisk radius `at` pt
        hln = upd $ locStraightLine (hvec $ xmin+xmaj) `at` pt .-^ hvec xmin
        vln = upd $ locStraightLine (vvec $ ymin+ymaj) `at` pt .-^ vvec ymin
        bdr = upd $ strokedRectangle (xmin+xmaj) (ymin+ymaj) `at` bl
    in bdr `oplus` hln `oplus` vln `oplus` dot


--------------------------------------------------------------------------------
-- Combining PosObject


instance (Fractional u, Ord u) => ZConcat (PosObject u) where
  superior = oplus
  anterior = flip oplus


instance (Num u, Ord u) => Concat (PosObject u) where
  hconcat = genMoveAlign spinemoveH spineRight
  vconcat = genMoveAlign spinemoveV spineBelow

instance (Num u, Ord u) => CatSpace (PosObject u) where
  hspace = genMoveSepH spinemoveH spineRight
  vspace = genMoveSepV spinemoveV spineBelow



instance (Fractional u, Ord u) => Align (PosObject u) where
  halign HTop    = genMoveAlign binmoveHTop    halignTopO
  halign HCenter = genMoveAlign binmoveHCenter halignCenterO
  halign HBottom = genMoveAlign binmoveHBottom halignBottomO

  valign VLeft   = genMoveAlign binmoveVLeft   valignLeftO
  valign VCenter = genMoveAlign binmoveVCenter valignCenterO
  valign VRight  = genMoveAlign binmoveVRight  valignRightO



genMoveAlign :: (Num u)   
             => (Orientation u -> Orientation u -> Vec2 u) 
             -> (Orientation u -> Orientation u -> Orientation u) 
             -> PosObject u -> PosObject u -> PosObject u
genMoveAlign mkV mkO po0 po1 = PosObject body
  where
   body = drawingCtx >>= \ctx -> 
          let (ortt0,pf0) = runCF ctx (getPosObject po0)
              (ortt1,pf1) = runCF ctx (getPosObject po1)
              v1          = mkV ortt0 ortt1
              ortt        = mkO ortt0 ortt1
              pf          = \pt -> pf0 pt `oplus` (pf1 $ pt .+^ v1)
          in return (ortt,pf)


--------------------------------------------------------------------------------
-- Sep

instance (Fractional u, Ord u) => AlignSpace (PosObject u) where
  halignSpace HTop    = genMoveSepH binmoveHTop    halignTopO
  halignSpace HCenter = genMoveSepH binmoveHCenter halignCenterO
  halignSpace HBottom = genMoveSepH binmoveHBottom halignBottomO

  valignSpace VLeft   = genMoveSepV binmoveVLeft   valignLeftO
  valignSpace VCenter = genMoveSepV binmoveVCenter valignCenterO
  valignSpace VRight  = genMoveSepV binmoveVRight  valignRightO


genMoveSepH :: (Num u)   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject u -> PosObject u -> PosObject u
genMoveSepH mkV mkO sep po0 po1  = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt0,pf0) = runCF ctx (getPosObject po0)
               (ortt1,pf1) = runCF ctx (getPosObject po1)
               v1          = hvec sep ^+^ mkV ortt0 ortt1
               ortt        = extendORight sep $ mkO ortt0 ortt1
               pf          = \pt -> pf0 pt `oplus` (pf1 $ pt .+^ v1)
           in return (ortt,pf)


genMoveSepV :: (Num u)   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject u -> PosObject u -> PosObject u
genMoveSepV mkV mkO sep po0 po1 = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt0,pf0) = runCF ctx (getPosObject po0)
               (ortt1,pf1) = runCF ctx (getPosObject po1)
               v1          = vvec (-sep) ^+^ mkV ortt0 ortt1
               ortt        = extendODown sep $ mkO ortt0 ortt1
               pf          = \pt -> pf0 pt `oplus` (pf1 $ pt .+^ v1)
           in return (ortt,pf)
