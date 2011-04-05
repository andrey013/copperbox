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


  -- * Concat
  , hcatPO
  , vcatPO

  , hcatBottomPO
  , hcatCenterPO
  , hcatTopPO

  , vcatLeftPO
  , vcatCenterPO
  , vcatRightPO 

  -- * Sep 
  , hsepPO
  , vsepPO

  , hsepBottomPO
  , hsepCenterPO
  , hsepTopPO

  , vsepLeftPO
  , vsepCenterPO
  , vsepRightPO 

  , halignPO
  , valignPO

  , halignSepPO
  , valignSepPO

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Bounded
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Orientation

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative

-- | Helper for PosObject - a LocImage that is /pre-applied/ to 
-- the DrawingContext.
--
-- This is somewhat contrived, but the orientation and the result
-- graphic from a PosImage have to be generated within the same 
-- DrawingContext.
--
type CtxFreeLocGraphic u = Point2 u -> GraphicAns u


-- | A positionable \"Object\" that is drawn as a 
-- 'BoundedLocGraphic'.
--
newtype PosObject u = PosObject
         { getPosObject :: CF (Orientation u, CtxFreeLocGraphic u) }


    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject = PosObject Double




type LocRectQuery u a = CF2 (Point2 u) RectAddress a

type BoundedLocRectGraphic u = LocRectQuery u (ImageAns BoundingBox u)

--------------------------------------------------------------------------------


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
makePosObject qortt img = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let ortt = runCF qortt ctx
               pf   = runCF1 img ctx
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
           let a    = runCF qy ctx
               ortt = runCF (mkO a) ctx
               pf   = runCF1 (mkG a) ctx
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
                          in replaceAns bb $ pure $ ptf $ displaceVec sv pt



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
           let (ortt,ptf) = runCF (getPosObject po) ctx
               deco       = runCF1 (fn ortt) ctx
           in return (ortt, ptf `oplus` deco)

-- | ante-eloborate
--
aelaboratePO :: (Orientation u -> LocGraphic u) -> PosObject u -> PosObject u
aelaboratePO fn po = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt,ptf) = runCF (getPosObject po) ctx
               deco       = runCF1 (fn ortt) ctx
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
           let (o0,pf0) = runCF (getPosObject po) ctx
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
           let (o0,pf0) = runCF (getPosObject po) ctx
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
    decorate (pure $ ptf pt) (illustrateOrientation ortt `at` pt)


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



hcatPO :: (Num u, Ord u)   
       => PosObject u -> PosObject u -> PosObject u
hcatPO = genMoveAlign spinemoveH spineRight


vcatPO :: (Num u, Ord u)   
       => PosObject u -> PosObject u -> PosObject u
vcatPO = genMoveAlign spinemoveV spineBelow



hcatBottomPO :: (Num u, Ord u)   
             => PosObject u -> PosObject u -> PosObject u
hcatBottomPO = genMoveAlign binmoveHBottom halignBottomO


hcatCenterPO :: (Fractional u, Ord u)   
             => PosObject u -> PosObject u -> PosObject u
hcatCenterPO = genMoveAlign binmoveHCenter halignCenterO


hcatTopPO :: (Num u, Ord u)   
          => PosObject u -> PosObject u -> PosObject u
hcatTopPO = genMoveAlign binmoveHTop halignTopO


vcatLeftPO :: (Fractional u, Ord u)   
           => PosObject u -> PosObject u -> PosObject u
vcatLeftPO = genMoveAlign binmoveVLeft valignLeftO


vcatCenterPO :: (Fractional u, Ord u)   
                => PosObject u -> PosObject u -> PosObject u
vcatCenterPO = genMoveAlign binmoveVCenter valignCenterO

vcatRightPO :: (Fractional u, Ord u)   
            => PosObject u -> PosObject u -> PosObject u
vcatRightPO = genMoveAlign binmoveVRight valignRightO


genMoveAlign :: (Num u)   
             => (Orientation u -> Orientation u -> Vec2 u) 
             -> (Orientation u -> Orientation u -> Orientation u) 
             -> PosObject u -> PosObject u -> PosObject u
genMoveAlign mkV mkO po0 po1 = PosObject body
  where
   body = drawingCtx >>= \ctx -> 
          let (ortt0,pf0) = runCF (getPosObject po0) ctx
              (ortt1,pf1) = runCF (getPosObject po1) ctx
              v1          = mkV ortt0 ortt1
              ortt        = mkO ortt0 ortt1
              pf          = \pt -> pf0 pt `oplus` (pf1 $ pt .+^ v1)
          in return (ortt,pf)


--------------------------------------------------------------------------------
-- Sep

hsepPO :: (Num u, Ord u)   
       => u -> PosObject u -> PosObject u -> PosObject u
hsepPO = genMoveSepH spinemoveH spineRight


vsepPO :: (Num u, Ord u)   
       => u -> PosObject u -> PosObject u -> PosObject u
vsepPO = genMoveSepV spinemoveV spineBelow


hsepBottomPO :: (Num u, Ord u)   
             => u -> PosObject u -> PosObject u -> PosObject u
hsepBottomPO = genMoveSepH binmoveHBottom halignBottomO


hsepCenterPO :: (Fractional u, Ord u)   
             => u -> PosObject u -> PosObject u -> PosObject u
hsepCenterPO = genMoveSepH binmoveHCenter halignCenterO


hsepTopPO :: (Num u, Ord u)   
          => u -> PosObject u -> PosObject u -> PosObject u
hsepTopPO = genMoveSepH binmoveHTop halignTopO


vsepLeftPO :: (Fractional u, Ord u)   
           => u -> PosObject u -> PosObject u -> PosObject u
vsepLeftPO = genMoveSepV binmoveVLeft valignLeftO


vsepCenterPO :: (Fractional u, Ord u)   
             => u -> PosObject u -> PosObject u -> PosObject u
vsepCenterPO = genMoveSepV binmoveVCenter valignCenterO

vsepRightPO :: (Fractional u, Ord u)   
            => u -> PosObject u -> PosObject u -> PosObject u
vsepRightPO = genMoveSepV binmoveVRight valignRightO


genMoveSepH :: (Num u)   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject u -> PosObject u -> PosObject u
genMoveSepH mkV mkO sep po0 po1  = PosObject body
  where
    body = drawingCtx >>= \ctx -> 
           let (ortt0,pf0) = runCF (getPosObject po0) ctx
               (ortt1,pf1) = runCF (getPosObject po1) ctx
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
           let (ortt0,pf0) = runCF (getPosObject po0) ctx
               (ortt1,pf1) = runCF (getPosObject po1) ctx
               v1          = vvec (-sep) ^+^ mkV ortt0 ortt1
               ortt        = extendODown sep $ mkO ortt0 ortt1
               pf          = \pt -> pf0 pt `oplus` (pf1 $ pt .+^ v1)
           in return (ortt,pf)

halignPO :: (Fractional u, Ord u)   
         => PosObject u -> HAlign -> [PosObject u] -> PosObject u
halignPO alt _  []     = alt
halignPO _   ha (x:xs) = go x xs
  where
    cat = case ha of HTop    -> hcatTopPO 
                     HCenter -> hcatCenterPO 
                     _       -> hcatBottomPO
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


valignPO :: (Fractional u, Ord u)   
         => PosObject u -> VAlign -> [PosObject u] -> PosObject u
valignPO alt _  []     = alt
valignPO _   va (x:xs) = go x xs
  where
    cat = case va of VLeft   -> vcatLeftPO 
                     VCenter -> vcatCenterPO 
                     _       -> vcatRightPO
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


halignSepPO :: (Fractional u, Ord u)   
            => PosObject u -> HAlign -> u -> [PosObject u] -> PosObject u
halignSepPO alt _  _ []     = alt
halignSepPO _   ha du (x:xs) = go x xs
  where
    cat = case ha of HTop    -> hsepTopPO du
                     HCenter -> hsepCenterPO du
                     _       -> hsepBottomPO du
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


valignSepPO :: (Fractional u, Ord u)   
         => PosObject u -> VAlign -> u -> [PosObject u] -> PosObject u
valignSepPO alt _  _  []     = alt
valignSepPO _   va du (x:xs) = go x xs
  where
    cat = case va of VLeft   -> vsepLeftPO du 
                     VCenter -> vsepCenterPO du 
                     _       -> vsepRightPO du
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


