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
  , PosGraphicObject
  
  , DPosObject
  , DPosGraphicObject

  -- * Operations
  , startPos
  , atStartPos

  , makePosObject
  , runPosObject

  , emptyPosGraphicObject

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
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Orientation

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace



-- | A positionable \"Object\" that is drawn as a 'LocImage'.
--
data PosObject t u = PosObject
      { pi_object_pos          :: Orientation u
      , pi_loc_image           :: LocImage t u
      }



-- | A 'PosObject' that draws a 'Graphic'.
--
type PosGraphicObject u = PosObject UNil u
    

    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject t = PosObject t Double


-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphicObject = PosGraphicObject Double


-- type PosQuery u ans = CF2 (Point2 u) RectAddress ans





--------------------------------------------------------------------------------



infixr 1 `startPos`

-- | 'startPos' : @ pos_image * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosObject' to a 'LocImage' by supplying it 
-- with a 'RectAddress' (start position).
--  
startPos :: Floating u 
         => PosObject t u -> RectAddress -> LocImage t u
startPos img rpos = runPosObject rpos img
 



-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectAddress' (start position).
--  
atStartPos ::  Floating u 
           => PosObject t u -> Point2 u -> RectAddress -> Image t u
atStartPos img pt rpos = runPosObject rpos img `at` pt



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
makePosObject :: Orientation u -> LocImage t u -> PosObject t u
makePosObject opos img = 
    PosObject { pi_object_pos = opos
             , pi_loc_image  = img
             }

runPosObject :: Fractional u 
            => RectAddress -> PosObject t u -> LocImage t u
runPosObject rpos (PosObject opos gf) =
    let sv = orientationStart rpos opos in moveStart (displaceVec sv) gf


-- | 'emptyPosGraphicObject' : @ PosGraphicObject @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosGraphicObject :: InterpretUnit u => PosGraphicObject u
emptyPosGraphicObject = makePosObject (Orientation 0 0 0 0) emptyLocGraphic


--------------------------------------------------------------------------------

illustratePosObject :: InterpretUnit u 
                   => PosObject t u -> LocImage t u
illustratePosObject (PosObject opos gf) = 
    decorate gf (illustrateOrientation opos)

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


--
-- NOTE - CONCATENATION
--
-- PosObject (currently) is not the right object to support the
-- concatenation that it should.
--


hcatPO :: (Num u, Ord u, OPlus (t u))   
       => PosObject t u -> PosObject t u -> PosObject t u
hcatPO = genMoveAlign spinemoveH spineRight


vcatPO :: (Num u, Ord u, OPlus (t u))   
       => PosObject t u -> PosObject t u -> PosObject t u
vcatPO = genMoveAlign spinemoveV spineAbove



hcatBottomPO :: (Num u, Ord u, OPlus (t u))   
             => PosObject t u -> PosObject t u -> PosObject t u
hcatBottomPO = genMoveAlign binmoveHBottom alignBottomR


hcatCenterPO :: (Fractional u, Ord u, OPlus (t u))   
             => PosObject t u -> PosObject t u -> PosObject t u
hcatCenterPO = genMoveAlign binmoveHCenter alignCenterR


hcatTopPO :: (Num u, Ord u, OPlus (t u))   
          => PosObject t u -> PosObject t u -> PosObject t u
hcatTopPO = genMoveAlign binmoveHTop alignTopR


vcatLeftPO :: (Fractional u, Ord u, OPlus (t u))   
           => PosObject t u -> PosObject t u -> PosObject t u
vcatLeftPO = genMoveAlign binmoveVLeft alignLeftU


vcatCenterPO :: (Fractional u, Ord u, OPlus (t u))   
                => PosObject t u -> PosObject t u -> PosObject t u
vcatCenterPO = genMoveAlign binmoveVCenter alignCenterU

vcatRightPO :: (Fractional u, Ord u, OPlus (t u))   
            => PosObject t u -> PosObject t u -> PosObject t u
vcatRightPO = genMoveAlign binmoveVRight alignRightU


genMoveAlign :: (Num u, OPlus (t u))   
             => (Orientation u -> Orientation u -> Vec2 u) 
             -> (Orientation u -> Orientation u -> Orientation u) 
             -> PosObject t u -> PosObject t u -> PosObject t u
genMoveAlign mkV mkOP (PosObject opos0 g0) (PosObject opos1 g1) = 
   let v1   = mkV  opos0 opos1 
       opos = mkOP opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec v1) g1
   in PosObject opos gf    


--------------------------------------------------------------------------------
-- Sep

hsepPO :: (Num u, Ord u, OPlus (t u))   
       => u -> PosObject t u -> PosObject t u -> PosObject t u
hsepPO = genMoveSepH spinemoveH spineRight


vsepPO :: (Num u, Ord u, OPlus (t u))   
       => u -> PosObject t u -> PosObject t u -> PosObject t u
vsepPO = genMoveSepV spinemoveV spineAbove


hsepBottomPO :: (Num u, Ord u, OPlus (t u))   
             => u -> PosObject t u -> PosObject t u -> PosObject t u
hsepBottomPO = genMoveSepH binmoveHBottom alignBottomR


hsepCenterPO :: (Fractional u, Ord u, OPlus (t u))   
             => u -> PosObject t u -> PosObject t u -> PosObject t u
hsepCenterPO = genMoveSepH binmoveHCenter alignCenterR


hsepTopPO :: (Num u, Ord u, OPlus (t u))   
          => u -> PosObject t u -> PosObject t u -> PosObject t u
hsepTopPO = genMoveSepH binmoveHTop alignTopR


vsepLeftPO :: (Fractional u, Ord u, OPlus (t u))   
           => u -> PosObject t u -> PosObject t u -> PosObject t u
vsepLeftPO = genMoveSepV binmoveVLeft alignLeftU


vsepCenterPO :: (Fractional u, Ord u, OPlus (t u))   
             => u -> PosObject t u -> PosObject t u -> PosObject t u
vsepCenterPO = genMoveSepV binmoveVCenter alignCenterU

vsepRightPO :: (Fractional u, Ord u, OPlus (t u))   
            => u -> PosObject t u -> PosObject t u -> PosObject t u
vsepRightPO = genMoveSepV binmoveVRight alignRightU


genMoveSepH :: (Num u, OPlus (t u))   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject t u -> PosObject t u -> PosObject t u
genMoveSepH mkV mkOP sep (PosObject opos0 g0) (PosObject opos1 g1) = 
   let v1   = mkV  opos0 opos1 
       opos = extendORight sep $ mkOP opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec $ hvec sep ^+^ v1) g1
   in PosObject opos gf    


genMoveSepV :: (Num u, OPlus (t u))   
            => (Orientation u -> Orientation u -> Vec2 u) 
            -> (Orientation u -> Orientation u -> Orientation u) 
            -> u
            -> PosObject t u -> PosObject t u -> PosObject t u
genMoveSepV mkV mkOP sep (PosObject opos0 g0) (PosObject opos1 g1) = 
   let v1   = mkV  opos0 opos1 
       opos = extendOUp sep $ mkOP opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec $ vvec sep ^+^ v1) g1
   in PosObject opos gf    



halignPO :: (Fractional u, Ord u, OPlus (t u))   
         => PosObject t u -> HAlign -> [PosObject t u] -> PosObject t u
halignPO alt _  []     = alt
halignPO _   ha (x:xs) = go x xs
  where
    cat = case ha of HTop    -> hcatTopPO 
                     HCenter -> hcatCenterPO 
                     _       -> hcatBottomPO
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


valignPO :: (Fractional u, Ord u, OPlus (t u))   
         => PosObject t u -> VAlign -> [PosObject t u] -> PosObject t u
valignPO alt _  []     = alt
valignPO _   va (x:xs) = go x xs
  where
    cat = case va of VLeft   -> vcatLeftPO 
                     VCenter -> vcatCenterPO 
                     _       -> vcatRightPO
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


halignSepPO :: (Fractional u, Ord u, OPlus (t u))   
            => PosObject t u -> HAlign -> u -> [PosObject t u] -> PosObject t u
halignSepPO alt _  _ []     = alt
halignSepPO _   ha du (x:xs) = go x xs
  where
    cat = case ha of HTop    -> hsepTopPO du
                     HCenter -> hsepCenterPO du
                     _       -> hsepBottomPO du
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys


valignSepPO :: (Fractional u, Ord u, OPlus (t u))   
         => PosObject t u -> VAlign -> u -> [PosObject t u] -> PosObject t u
valignSepPO alt _  _  []     = alt
valignSepPO _   va du (x:xs) = go x xs
  where
    cat = case va of VLeft   -> vsepLeftPO du 
                     VCenter -> vsepCenterPO du 
                     _       -> vsepRightPO du
    go acc []     = acc
    go acc (y:ys) = go (cat acc y) ys
