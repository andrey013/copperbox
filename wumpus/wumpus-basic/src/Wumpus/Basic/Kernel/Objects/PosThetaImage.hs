{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- PosImage object extended with angle of inclination.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosThetaImage
  (

  -- * Positionable image


    PosThetaImage
  , DPosThetaImage

  , PosThetaGraphic
  , DPosThetaGraphic



  , runPosThetaImage
  , rawPosThetaImage

  , atStartPosRot
  , startPosRot
  , ptRot
  
  , promote_pti1
  , promote_pti2
  , promote_pti3
  , lift_pti1
  , lift_pti2
  , lift_pti3

  , bindQuery_pti

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.PosImage
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid





-- | A positionable Image that supports drawing at some angle of 
-- inclination.
-- 
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
newtype PosThetaImage t u = PosThetaImage {
    getPosThetaImage :: DrawingContext -> Point2 u -> 
                        RectPosition   -> Radian   -> (t u, CatPrim) }
    
-- | Version of PosThetaImage specialized to Double for the unit type.
--
type DPosThetaImage t = PosThetaImage t Double



-- | A positionable Graphic that supports drawing at some angle of
-- inclination.
--
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
type PosThetaGraphic u = PosThetaImage UNil u
    
-- | Version of PosThetaGraphic specialized to Double for the unit type.
--
type DPosThetaGraphic = PosThetaGraphic Double






--------------------------------------------------------------------------------

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapPosThetaImage :: (t u -> t' u) -> (Primitive -> Primitive) 
                   -> PosThetaImage t u -> PosThetaImage t' u
bimapPosThetaImage l r gf = PosThetaImage $ \ctx pt rpos ang -> 
    bimap l (cpmap r) $ getPosThetaImage gf ctx pt rpos ang


instance Object PosThetaImage where
  local_ctx     = localPosThetaImg
  ignoreAns     = bimapPosThetaImage (const UNil) id
  replaceAns o  = bimapPosThetaImage (const o) id
  mapAns f      = bimapPosThetaImage f id
  hyperlink hyp = bimapPosThetaImage id (xlinkPrim hyp)
  annotate      = annoPosThetaImg
  decorate      = decoPosThetaImg
  bind          = bindPosThetaImg
  unit          = unitPosThetaImg


localPosThetaImg :: (DrawingContext -> DrawingContext) 
                 -> PosThetaImage t u 
                 -> PosThetaImage t u
localPosThetaImg upd gf = PosThetaImage $ \ctx pt rpos ang -> 
    getPosThetaImage gf (upd ctx) pt rpos ang



decoPosThetaImg :: PosThetaImage t u 
                -> PosThetaGraphic u 
                -> PosThetaImage t u
decoPosThetaImg fa fb = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage fa ctx pt rpos ang
        (_,o2) = getPosThetaImage fb ctx pt rpos ang
    in (a, o1 `oplus` o2)
                        
annoPosThetaImg :: PosThetaImage t u 
                -> (t u -> PosThetaGraphic u) 
                -> PosThetaImage t u
annoPosThetaImg fa mf = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage fa ctx pt rpos ang
        (_,o2) = getPosThetaImage (mf a) ctx pt rpos ang
    in (a, o1 `oplus` o2)


bindPosThetaImg :: PosThetaImage t u 
                -> (t u -> PosThetaImage t1 u) 
                -> PosThetaImage t1 u
bindPosThetaImg gf fn = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage gf ctx pt rpos ang
        (b,o2) = getPosThetaImage (fn a) ctx pt rpos ang
    in (b, o1 `oplus` o2)

unitPosThetaImg :: t u -> PosThetaImage t u
unitPosThetaImg a = PosThetaImage $ \_ _ _ _ -> (a, mempty)


instance MoveStart PosThetaImage where
  moveStart fn gf = PosThetaImage $ \ctx pt rpos ang -> 
                      getPosThetaImage gf ctx (fn pt) rpos ang




--------------------------------------------------------------------------------
-- builders and destructors


runPosThetaImage :: PosThetaImage t u -> DrawingContext 
                 -> Point2 u -> RectPosition 
                 -> Radian
                 -> (t u, CatPrim)
runPosThetaImage gf ctx pt rpos ang = getPosThetaImage gf ctx pt rpos ang

-- This seems to be the one for down casting...
-- 
rawPosThetaImage :: (DrawingContext -> Point2 u -> RectPosition 
                                    -> Radian   ->  (t u, CatPrim)) 
                 -> PosThetaImage t u
rawPosThetaImage fn = PosThetaImage $ \ctx pt rpos ang -> fn ctx pt rpos ang


-- should @rot@ be a overloaded?

{-

infixr 1 `startPos`

-- | 'startPos' : @ pos_image * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosImage' to a 'LocImage' by supplying it 
-- with a 'RectPosition' (start position).
--  
startPos :: Floating u 
         => PosImage t u -> RectPosition -> LocImage t u
startPos gf rpos = rawLocImage (\ctx pt -> getPosImage gf ctx pt rpos) 
 

-}

-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPosRot :: PosThetaImage t u -> Point2 u -> RectPosition -> Radian 
              -> Image t u
atStartPosRot gf pt rpos ang = 
    rawImage $ \ctx -> getPosThetaImage gf ctx pt rpos ang


startPosRot :: PosThetaImage t u -> RectPosition -> Radian -> LocImage t u
startPosRot gf rpos ang = 
    rawLocImage $ \ctx pt -> getPosThetaImage gf ctx pt rpos ang


infixr 1 `ptRot`

ptRot :: PosThetaImage t u -> Radian -> PosImage t u
ptRot gf ang = 
    rawPosImage $ \ctx pt rpos -> getPosThetaImage gf ctx pt rpos ang





promote_pti1 :: (Radian -> PosImage t u) -> PosThetaImage t u
promote_pti1 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runPosImage (gf ang) ctx pt rpos


promote_pti2 :: (RectPosition -> Radian -> LocImage t u) 
             -> PosThetaImage t u
promote_pti2 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runLocImage (gf rpos ang) ctx pt


promote_pti3 :: (Point2 u -> RectPosition -> Radian -> Image t u) 
             -> PosThetaImage t u
promote_pti3 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runImage (gf pt rpos ang) ctx


lift_pti1 :: PosImage t u -> PosThetaImage t u
lift_pti1 gf = PosThetaImage $ \ctx pt rpos _ -> runPosImage gf ctx pt rpos


lift_pti2 :: LocImage t u -> PosThetaImage t u
lift_pti2 gf = PosThetaImage $ \ctx pt _ _ -> runLocImage gf ctx pt

lift_pti3 :: Image t u -> PosThetaImage t u
lift_pti3 gf = PosThetaImage $ \ctx _ _ _ -> runImage gf ctx 


-- TODO - what is the constructor function needed here?


-- | WARNING - not correct... ignores rpos...
--
bindQuery_pti :: Query ans 
              -> (ans -> PosThetaImage t u) 
              -> PosThetaImage t u
bindQuery_pti qry fn = PosThetaImage $ \ctx pt rpos ang -> 
    let ans = runQuery qry ctx in runPosThetaImage (fn ans) ctx pt rpos ang
