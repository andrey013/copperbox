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


  -- * Operations
  , startPos
  , atStartPos

  , makePosImage
  , makePosImage2 
  , runPosImage2

  , illustratePosImage

  , hcatPI
  , vcatPI

  , hcatBottomPI
  , hcatCenterPI
  , hcatTopPI

  , vcatLeftPI
  , vcatCenterPI
  , vcatRightPI 

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
import Wumpus.Basic.Kernel.Objects.ObjectPos

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
    let v1 = objectPosStart rpos opos in gf `at` displaceVec v1 start


makePosImage2 :: Fractional u 
              => ObjectPos u -> LocImage t u -> PosImage2 t u
makePosImage2 opos img = 
    PosImage { pi_object_pos = opos
             , pi_loc_image  = img
             }

runPosImage2 :: Fractional u 
             => RectPosition -> PosImage2 t u -> LocImage t u
runPosImage2 rpos (PosImage opos gf) =
    let sv = objectPosStart rpos opos in moveStart (displaceVec sv) gf


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
-- Combining PosImage


--
-- NOTE - CONCATENATION
--
-- PosImage (currently) is not the right object to support the
-- concatenation that it should.
--


hcatPI :: (Num u, Ord u, OPlus (t u))   
       => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatPI = genMoveAlign spinemoveH spineRight


vcatPI :: (Num u, Ord u, OPlus (t u))   
       => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
vcatPI = genMoveAlign spinemoveV spineAbove


hcatBottomPI :: (Num u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatBottomPI = genMoveAlign binmoveHBottom alignBottomR


hcatCenterPI :: (Fractional u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatCenterPI = genMoveAlign binmoveHCenter alignCenterR


hcatTopPI :: (Num u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
hcatTopPI = genMoveAlign binmoveHTop alignTopR


vcatLeftPI :: (Fractional u, Ord u, OPlus (t u))   
           => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
vcatLeftPI = genMoveAlign binmoveVLeft alignLeftU


vcatCenterPI :: (Fractional u, Ord u, OPlus (t u))   
                => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
vcatCenterPI = genMoveAlign binmoveVCenter alignCenterU

vcatRightPI :: (Fractional u, Ord u, OPlus (t u))   
            => PosImage2 t u -> PosImage2 t u -> PosImage2 t u
vcatRightPI = genMoveAlign binmoveVRight alignRightU


genMoveAlign :: (Num u, OPlus (t u))   
             => (ObjectPos u -> ObjectPos u -> Vec2 u) 
             -> (ObjectPos u -> ObjectPos u -> ObjectPos u) 
             -> PosImage2 t u -> PosImage2 t u -> PosImage2 t u
genMoveAlign mkV mkOP (PosImage opos0 g0) (PosImage opos1 g1) = 
   let v1   = mkV  opos0 opos1 
       opos = mkOP opos0 opos1
       gf   = g0 `oplus` moveStart (displaceVec v1) g1
   in PosImage opos gf    
