{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
newtype PosThetaImage r u = PosThetaImage {
    getPosThetaImage :: DrawingContext -> Point2 u -> 
                        RectPosition   -> Radian   -> (r u, CatPrim) }


type instance Answer (PosThetaImage r u)     = r u
type instance ArgDiff (PosThetaImage r u) (Image r u) 
    = (Point2 u, RectPosition, Radian)



type PosThetaQuery u ans = Query (Point2 u -> RectPosition -> Radian -> ans)
    
-- | Version of PosThetaImage specialized to Double for the unit type.
--
type DPosThetaImage r = PosThetaImage r Double



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


instance PromoteR3 (PosThetaImage r u) (Image r u) where
  promoteR3 = promote_pti3


instance BindQuery (PosThetaImage r u) where
   (&=>) = bindQuery


instance BindQueryR3 (PosThetaImage r u) (Image r u) where
   (&====>) = bindR3



--------------------------------------------------------------------------------

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapPosThetaImage :: (r u -> r1 u) -> (Primitive -> Primitive) 
                   -> PosThetaImage r u -> PosThetaImage r1 u
bimapPosThetaImage l r gf = PosThetaImage $ \ctx pt rpos ang -> 
    bimap l (cpmap r) $ getPosThetaImage gf ctx pt rpos ang


instance Object PosThetaImage where
  local_ctx     = localPosThetaImg
  ignoreAns     = bimapPosThetaImage (const UNil) id
  replaceAns o  = bimapPosThetaImage (const o) id
  mapAns f      = bimapPosThetaImage f id
  hyperlink hyp = bimapPosThetaImage id (xlinkPrim hyp)
  clipObject pp = bimapPosThetaImage id (clip pp)
  annotate      = annoPosThetaImg
  decorate      = decoPosThetaImg
  bind          = bindPosThetaImg
  unit          = unitPosThetaImg


localPosThetaImg :: (DrawingContext -> DrawingContext) 
                 -> PosThetaImage r u 
                 -> PosThetaImage r u
localPosThetaImg upd gf = PosThetaImage $ \ctx pt rpos ang -> 
    getPosThetaImage gf (upd ctx) pt rpos ang



decoPosThetaImg :: PosThetaImage r u 
                -> PosThetaGraphic u 
                -> PosThetaImage r u
decoPosThetaImg fa fb = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage fa ctx pt rpos ang
        (_,o2) = getPosThetaImage fb ctx pt rpos ang
    in (a, o1 `oplus` o2)
                        
annoPosThetaImg :: PosThetaImage r u 
                -> (r u -> PosThetaGraphic u) 
                -> PosThetaImage r u
annoPosThetaImg fa mf = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage fa ctx pt rpos ang
        (_,o2) = getPosThetaImage (mf a) ctx pt rpos ang
    in (a, o1 `oplus` o2)


bindPosThetaImg :: PosThetaImage r u 
                -> (r u -> PosThetaImage r1 u) 
                -> PosThetaImage r1 u
bindPosThetaImg gf fn = PosThetaImage $ \ctx pt rpos ang -> 
    let (a,o1) = getPosThetaImage gf ctx pt rpos ang
        (b,o2) = getPosThetaImage (fn a) ctx pt rpos ang
    in (b, o1 `oplus` o2)

unitPosThetaImg :: r u -> PosThetaImage r u
unitPosThetaImg a = PosThetaImage $ \_ _ _ _ -> (a, mempty)


instance MoveStart PosThetaImage where
  moveStart fn gf = PosThetaImage $ \ctx pt rpos ang -> 
                      getPosThetaImage gf ctx (fn pt) rpos ang




--------------------------------------------------------------------------------
-- builders and destructors


runPosThetaImage :: PosThetaImage r u -> DrawingContext 
                 -> Point2 u -> RectPosition 
                 -> Radian
                 -> (r u, CatPrim)
runPosThetaImage gf ctx pt rpos ang = getPosThetaImage gf ctx pt rpos ang

-- This seems to be the one for down casting...
-- 
rawPosThetaImage :: (DrawingContext -> Point2 u -> RectPosition 
                                    -> Radian   ->  (r u, CatPrim)) 
                 -> PosThetaImage r u
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
         => PosImage r u -> RectPosition -> LocImage r u
startPos gf rpos = rawLocImage (\ctx pt -> getPosImage gf ctx pt rpos) 
 

-}

-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPosRot :: PosThetaImage r u -> Point2 u -> RectPosition -> Radian 
              -> Image r u
atStartPosRot gf pt rpos ang = 
    rawImage $ \ctx -> getPosThetaImage gf ctx pt rpos ang


startPosRot :: PosThetaImage r u -> RectPosition -> Radian -> LocImage r u
startPosRot gf rpos ang = 
    rawLocImage $ \ctx pt -> getPosThetaImage gf ctx pt rpos ang


infixr 1 `ptRot`

ptRot :: PosThetaImage r u -> Radian -> PosImage r u
ptRot gf ang = 
    rawPosImage $ \ctx pt rpos -> getPosThetaImage gf ctx pt rpos ang





promote_pti1 :: (Radian -> PosImage r u) -> PosThetaImage r u
promote_pti1 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runPosImage (gf ang) ctx pt rpos


promote_pti2 :: (RectPosition -> Radian -> LocImage r u) 
             -> PosThetaImage r u
promote_pti2 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runLocImage (gf rpos ang) ctx pt


promote_pti3 :: (Point2 u -> RectPosition -> Radian -> Image r u) 
             -> PosThetaImage r u
promote_pti3 gf = 
    PosThetaImage $ \ctx pt rpos ang -> runImage (gf pt rpos ang) ctx


lift_pti1 :: PosImage r u -> PosThetaImage r u
lift_pti1 gf = PosThetaImage $ \ctx pt rpos _ -> runPosImage gf ctx pt rpos


lift_pti2 :: LocImage r u -> PosThetaImage r u
lift_pti2 gf = PosThetaImage $ \ctx pt _ _ -> runLocImage gf ctx pt

lift_pti3 :: Image r u -> PosThetaImage r u
lift_pti3 gf = PosThetaImage $ \ctx _ _ _ -> runImage gf ctx 


-- TODO - what is the constructor function needed here?


bindQuery :: Query ans 
          -> (ans -> PosThetaImage r u) 
          -> PosThetaImage r u
bindQuery qry fn = PosThetaImage $ \ctx pt rpos ang -> 
    let ans = runQuery qry ctx in runPosThetaImage (fn ans) ctx pt rpos ang





-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
bindR3 :: PosThetaQuery u ans -> (ans -> Image r u) -> PosThetaImage r u
bindR3 qy fn = PosThetaImage $ \ctx pt rpos ang -> 
    let f1 = runQuery qy ctx in runImage (fn $ f1 pt rpos ang) ctx
