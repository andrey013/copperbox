{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosImage
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

module Wumpus.Basic.Kernel.Objects.PosImage
  (

  -- * Positionable image

    PosImage
  , PosGraphic

  , DPosImage 
  , DPosGraphic

  , PosQuery

  , PosImage2
  , PosGraphic2

  -- * Components
  , RectPosition(..)
  , ObjectPos(..)

  -- * Operations
  , startPos
  , atStartPos

  , makePosImage
  , makePosImage2 
  , runPosImage2

  , illustratePosImage

  , objectPosBounds

  , hcatPI
  , vcatPI

  , hcatAlignBottom
  , hcatAlignCenter
  , hcatAlignTop

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

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space




-- | A positionable 'LocImage'.
--
type PosImage t u = CF2 (Point2 u) RectPosition (ImageAns t u)


-- | A positionable 'LocGraphic'.
--
type PosGraphic u = PosImage UNil u
    

    
-- | Version of PosImage specialized to Double for the unit type.
--
type DPosImage t = PosImage t Double


-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphic = PosGraphic Double


type PosQuery u ans = CF2 (Point2 u) RectPosition ans



data PosImage2 t u = PosImage
      { pi_object_pos          :: ObjectPos u
      , pi_loc_image           :: LocImage t u
      }


type PosGraphic2 u = PosImage2 UNil u

--------------------------------------------------------------------------------


-- | Datatype enumerating positions within a rectangle that can be
-- derived for a 'PosGraphic'.  
--
data RectPosition = CENTER 
                  | NN | SS | EE | WW | NE | NW | SE | SW 
                  | BLL | BLC | BLR
  deriving (Enum,Eq,Ord,Show)


-- | Utility datatype representing orientation within a 
-- rectangular /frame/. ObjectPos is useful for graphics such as 
-- text where the start point is not necessarily at the center 
-- (or bottom left).
--
-- > x_minor is the horizontal distance from the left to the start point
-- >
-- > x_major is the horizontal distance from the start point to the right
-- >
-- > y_minor is the vertical distance from the bottom to the start point
-- >
-- > y_major is the vertical distance from the start point to the top
--
-- Values should be not be negative!
--
-- 
data ObjectPos u = ObjectPos 
      { op_x_minor      :: !u
      , op_x_major      :: !u
      , op_y_minor      :: !u
      , op_y_major      :: !u
      }
  deriving (Eq,Ord,Show)




--------------------------------------------------------------------------------

instance Functor ObjectPos where
  fmap f (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos (f xmin) (f xmaj) (f ymin) (f ymaj)

instance (Fractional u, Ord u) => OPlus (ObjectPos u) where
  oplus = concatObjectPos


-- | Concatenation here essentially turns both ObjectPos objects
-- into /center-form/ then finds the maximum rectangle.
--
concatObjectPos :: (Fractional u, Ord u) 
                => ObjectPos u -> ObjectPos u -> ObjectPos u
concatObjectPos op0 op1 = ObjectPos hw hw hh hh
  where
    (hw0,hh0) = halfDists op0
    (hw1,hh1) = halfDists op1
    hw        = max hw0 hw1
    hh        = max hh0 hh1




-- | Find the half-width and half-height of an ObjectPos.
-- 
-- Essentially this is /center-form/ of an ObjectPos, but 
-- in /center-form/ there is duplication: 
--
-- > xminor == xmajor
-- > yminor == ymajor
-- 
-- So instead, the result type is just a pair.
--
halfDists :: Fractional u => ObjectPos u -> (u,u)
halfDists (ObjectPos xmin xmaj ymin ymaj) = 
    (0.5 * (xmin+xmaj), 0.5 * (ymin+ymaj))



infixr 1 `startPos`

-- | 'startPos' : @ pos_image * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosImage' to a 'LocImage' by supplying it 
-- with a 'RectPosition' (start position).
--  
startPos :: Floating u 
         => PosImage t u -> RectPosition -> LocImage t u
startPos = apply1R2
 



-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPos ::  Floating u 
           => PosImage t u -> Point2 u -> RectPosition -> Image t u
atStartPos = apply2R2 



--------------------------------------------------------------------------------


-- | 'makePosImage' : @ object_pos * loc_image -> PosImage @ 
--
-- Create a 'PosImage' from an 'ObjectPos' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
-- This is the /primary/ constructor for PosImages. Because the
-- PosImage type is considered as a specialized object it does
-- not have the range of functions of LocImage or LocThetaImage.
-- 
makePosImage :: Fractional u 
             => ObjectPos u -> LocImage t u -> PosImage t u
makePosImage opos gf = promoteR2 $ \start rpos -> 
    let v1 = startVector rpos opos in gf `at` displaceVec v1 start


makePosImage2 :: Fractional u 
              => ObjectPos u -> LocImage t u -> PosImage2 t u
makePosImage2 opos img = 
    PosImage { pi_object_pos = opos
             , pi_loc_image  = img
             }

runPosImage2 :: Fractional u 
             => RectPosition -> PosImage2 t u -> LocImage t u
runPosImage2 rpos (PosImage opos gf) =
    let sv = startVector rpos opos in moveStart (displaceVec sv) gf

-- | The vector from some Rectangle position to the start point.
--
startVector :: Fractional u => RectPosition -> ObjectPos u -> Vec2 u
startVector rpos (ObjectPos xminor xmajor yminor ymajor) = go rpos
  where
    w         = xminor + xmajor
    h         = yminor + ymajor
    hw        = 0.5 * w
    hh        = 0.5 * h
   
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 ((-hw) + xminor) ((-hh) + yminor)
    go NN     = V2 ((-hw) + xminor) ((-h)  + yminor)
    go SS     = V2 ((-hw) + xminor)   yminor
    go EE     = V2 ((-w)  + xminor) ((-hh) + yminor)
    go WW     = V2 xminor           ((-hh) + yminor)
    go NE     = V2 (-xmajor)        (-ymajor)
    go SE     = V2 (-xmajor)          yminor
    go SW     = V2 xminor             yminor
    go NW     = V2 xminor           (-ymajor)
    go BLL    = V2 xminor             0
    go BLC    = V2 ((-hw) + xminor)   0
    go BLR    = V2 ((-w)  + xminor)   0 

-- | Calculate the bounding box formed by locating the 'ObjectPos'
-- at the supplied point.
-- 
objectPosBounds :: Fractional u 
                => Point2 u -> RectPosition -> ObjectPos u -> BoundingBox u
objectPosBounds (P2 x y) pos (ObjectPos xmin xmaj ymin ymaj) = go pos
  where
    w         = xmin + xmaj
    h         = ymin + ymaj
    hw        = 0.5 * w
    hh        = 0.5 * h
    bbox      = \bl -> BBox bl (bl .+^ vec w h)

    -- go finds the bottom-left corner...

    go CENTER = bbox $ P2 (x-hw) (y-hh)
    go NN     = bbox $ P2 (x-hw) (y-h)
    go SS     = bbox $ P2 (x-hw)  y
    go EE     = bbox $ P2 (x-w)  (y-hh)
    go WW     = bbox $ P2  x     (y-hh)
    go NE     = bbox $ P2 (x-w)  (y-h)
    go SE     = bbox $ P2 (x-w)   y
    go SW     = bbox $ P2 x       y
    go NW     = bbox $ P2 x      (y-h)
    go BLL    = bbox $ P2 x       ymin
    go BLC    = bbox $ P2 (x-hw)  ymin
    go BLR    = bbox $ P2 (x-w)   ymin


illustratePosImage :: InterpretUnit u 
                   => PosImage2 t u -> LocImage t u
illustratePosImage (PosImage opos gf) = 
    decorate gf (illustrateObjectPos opos)

illustrateObjectPos :: InterpretUnit u 
                    => ObjectPos u -> LocGraphic u
illustrateObjectPos (ObjectPos xmin xmaj ymin ymaj) = promoteR1 $ \pt -> 
    dinterpCtx 3 >>= \radius -> 
    let upd = localize (fill_colour blue . dotted_line)
        bl  = pt .-^ V2 xmin ymin
        dot = localize (fill_colour red) $ filledDisk radius `at` pt
        hln = upd $ locStraightLine (hvec $ xmin+xmaj) `at` pt .-^ hvec xmin
        vln = upd $ locStraightLine (vvec $ ymin+ymaj) `at` pt .-^ vvec ymin
        bdr = upd $ strokedRectangle (xmin+xmaj) (ymin+ymaj) `at` bl
    in bdr `oplus` hln `oplus` vln `oplus` dot

--------------------------------------------------------------------------------
-- Combining ObjectPos

-- Note - there are lots of concatenations (due to alignment) 
-- we need a consistent name scheme...


-- | Second ObjectPos is moved /to the right/ of the first along
-- the /spine/ i.e the baseline.
--
spineRight :: (Num u, Ord u) 
            => ObjectPos u -> ObjectPos u -> ObjectPos u
spineRight (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
           (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    ObjectPos { op_x_minor = xmin0
              , op_x_major = xmaj0 + xmin1 + xmaj1 
              , op_y_minor = max ymin0 ymin1
              , op_y_major = max ymaj0 ymaj1
              }


-- | Second ObjectPos is moved /above/ the first along the spine
-- i.e. the vertical point between the left minor and right major
-- (not the same as the horizontal center).
--
spineAbove :: (Num u, Ord u) 
           => ObjectPos u -> ObjectPos u -> ObjectPos u
spineAbove (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
           (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    ObjectPos { op_x_minor = max xmin0 xmin1
              , op_x_major = max xmaj0 xmaj1
              , op_y_minor = ymin0 
              , op_y_major = ymaj0 + ymin1 + ymaj1
              }



-- | xmin and xmaj same as left.
--
alignBottomR :: (Num u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignBottomR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
             (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = ymin0
                 , op_y_major = max ymaj0 (hr - ymin0)
                 }


-- | xmin same as left.
--
alignCenterR :: (Fractional u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignCenterR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
             (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hl         = ymin0 + ymaj0
        hr         = ymin1 + ymaj1
        half_diff  = 0.5 * (hr - hl)
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = if hl >= hr then ymin0 else (ymin0 + half_diff)
                 , op_y_major = if hl >= hr then ymaj0 else (ymaj0 + half_diff)
                 }



-- | xmin and ymaj same as left.
--
alignTopR :: (Num u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignTopR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
          (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = max ymin0 (hr - ymaj0)
                 , op_y_major = ymaj0
                 }


--------------------------------------------------------------------------------
-- Combining PosImage


--
-- NOTE - CONCATENATION
--
-- PosImage (currently) is not the right object to support the
-- concatenation that it should.
--


hcatPI :: (Num u, Ord u, OPlus (t u))   
       => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatPI (PosImage opos0 g0) (PosImage opos1 g1) = 
   let v1   = hvec (op_x_major opos0 + op_x_minor opos1)
       opos = spineRight opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec v1) g1
   in PosImage opos gf    


vcatPI :: (Num u, Ord u, OPlus (t u))   
       => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
vcatPI (PosImage opos0 g0) (PosImage opos1 g1) = 
   let v1   = vvec (op_y_major opos0 + op_y_minor opos1) 
       opos = spineAbove opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec v1) g1
   in PosImage opos gf    


hcatAlignBottom :: (Num u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatAlignBottom (PosImage opos0 g0) (PosImage opos1 g1) = 
   let hdist  = op_x_major opos0 + op_x_minor opos1
       vdist  = negate $ op_y_minor opos0 - op_y_minor opos1
       opos   = alignBottomR opos0 opos1
       gf     = g0 `oplus` moveStart (displaceVec $ V2 hdist vdist) g1
   in PosImage opos gf    


hcatAlignCenter :: (Fractional u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatAlignCenter (PosImage opos0 g0) (PosImage opos1 g1) = 
   let hdist  = op_x_major opos0 + op_x_minor opos1
       vdist  = op_y_major opos0 - op_y_major opos1
       opos   = alignCenterR opos0 opos1
       gf     = g0 `oplus` moveStart (displaceVec $ V2 hdist vdist) g1
   in PosImage opos gf    


hcatAlignTop :: (Num u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatAlignTop (PosImage opos0 g0) (PosImage opos1 g1) = 
   let hdist  = op_x_major opos0 + op_x_minor opos1
       vdist  = op_y_major opos0 - op_y_major opos1
       opos   = alignTopR opos0 opos1
       gf     = g0 `oplus` moveStart (displaceVec $ V2 hdist vdist) g1
   in PosImage opos gf    
