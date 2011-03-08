{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Image
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Image and Graphic types - these are functional types from the 
-- DrawingContext to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Image
   (
     Graphic
   , Image

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.BaseExts
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


-- | Graphic - function from the DrawingContext to a graphic 
-- /primitive/.
--
newtype Graphic u = Graphic { getGraphic :: DrawingContext -> Primitive }


-- | Graphic - function from the DrawingContext to a polymorphic 
-- /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype Image t u = Image { getImage :: DrawingContext -> (t u, Primitive) }



instance OPlus (Graphic u) where
  fa `oplus` fb = Graphic $ \ctx -> 
                    getGraphic fa ctx `oplus` getGraphic fb ctx

instance OPlus (t u) => OPlus (Image t u) where
  fa `oplus` fb = Image $ \ctx -> 
                    getImage fa ctx `oplus` getImage fb ctx


primMapGraphic :: (Primitive -> Primitive) -> Graphic u -> Graphic u
primMapGraphic fn gf = Graphic $ \ctx -> fn $ getGraphic gf ctx

bimapImage :: (t u -> t' u') -> (Primitive -> Primitive) 
           -> Image t u -> Image t' u'
bimapImage l r gf = Image $ \ctx -> bimap l r $ getImage gf ctx

-- For Graphic convert just strips and replaces the constructor.

instance UnitConvert Graphic where
  uconvert = Graphic . getGraphic 

-- This needs drawing context so cannot be done with 'ansMapImage'.
--
instance UnitConvertExt t => UnitConvert (Image t) where
  uconvert gf = Image $ \ctx -> let (a,o) = getImage gf ctx
                                    sz    = dc_font_size ctx
                                in (uconvertExt sz a, o)


instance Rotate (Graphic u) where
  rotate ang = primMapGraphic (rotate ang)

instance Scale (Graphic u) where
  scale sx sy = primMapGraphic (scale sx sy) 

instance RotateAbout (Graphic u) where
  rotateAbout ang pt = primMapGraphic (rotateAbout ang pt)

instance Translate (Graphic u) where
  translate dx dy = primMapGraphic (translate dx dy) 



instance Rotate (t u) => Rotate (Image t u) where
  rotate ang = bimapImage (rotate ang) (rotate ang)

instance Scale (t u) => Scale (Image t u) where
  scale sx sy = bimapImage (scale sx sy) (scale sx sy)

instance RotateAbout (t u) => RotateAbout (Image t u) where
  rotateAbout ang pt = bimapImage (rotateAbout ang pt) (rotateAbout ang pt)

instance Translate (t u) => Translate (Image t u) where
  translate dx dy = bimapImage (translate dx dy) (translate dx dy)


instance Hyperlink (Graphic u) where
  hyperlink hyp = primMapGraphic (xlinkPrim hyp)

instance Hyperlink (Image t u) where
  hyperlink hyp = bimapImage id (xlinkPrim hyp)



instance IgnoreAns Image Graphic where
  ignoreAns gf = Graphic $ \ctx -> snd $ getImage gf ctx
  replaceAns o = bimapImage (const o) id

instance Annotate Image Graphic where
  annotate = annoImg
  decorate = decoImg



annoImg :: Image t u -> Graphic u -> Image t u
annoImg fa fb = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        o2     = getGraphic fb ctx
    in (a,o1 `oplus` o2)
                        
decoImg :: Image t u -> (t u -> Graphic u) -> Image t u
decoImg fa mf = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        o2     = getGraphic (mf a) ctx
    in (a,o1 `oplus` o2)


{-





-- | Ignore the answer produced by an 'Image', a 'LocImage' etc.
--
-- Use this function to turn an 'Image' into a 'Graphic', a 
-- 'LocImage into a 'LocGraphic'.
--
ignoreAns :: Functor f => f (ImageAns t u) -> f (ImageAns (Const ()) u)
ignoreAns = fmap (bimapImageAns (const noAns) id)


-- | Replace the answer produced by an 'Image', a 'LocImage' etc.
--
replaceAns :: Functor f => (t1 u1) -> f (ImageAns t u) -> f (ImageAns t1 u1)
replaceAns a = fmap (bimapImageAns (const a) id)


-- | Apply the supplied function to the answer produced by an 
-- 'Image', a 'LocImage' etc.
--
mapAns :: Functor f => (t u -> t1 u1) -> f (ImageAns t u) -> f (ImageAns t1 u1)
mapAns f = fmap (bimapImageAns f id)


-- | 'intoImage' : @ context_function * graphic -> Image @
--
-- Build an 'Image' from a context function ('CF') that generates 
-- the answer and a 'Graphic' that draws the 'Image'.
--
intoImage :: CF (t u) -> Graphic u -> Image t u
intoImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)


-- | 'intoLocImage' : @ loc_context_function * loc_graphic -> LocImage @
--
-- /Loc/ version of 'intoImage'. 
-- 
-- The 'LocImage' is built as a function from an implicit start 
-- point to the answer.
--
intoLocImage :: LocCF u (t u) -> LocGraphic u -> LocImage t u
intoLocImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)

-- | 'intoLocThetaImage' : @ loc_theta_cf * loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaCF u (t u) -> LocThetaGraphic u -> LocThetaImage t u
intoLocThetaImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)








-- | 'decorate' : @ image * graphic -> Image @
-- 
-- Decorate an Image by super-imposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
-- > decorate :: Image t u -> Graphic u -> Image t u
-- > decorate :: LocImage t u -> LocGraphic u -> LocImage t u
-- > decorate :: LocThetaImage t u -> LocThetaGraphic u -> LocTheteImage t u
--
decorate :: Monad m 
         => m (ImageAns t u) -> m (ImageAns t0 u0) -> m (ImageAns t u) 
decorate img gf = 
    img >>= \a1 -> gf >>= \a2 -> 
    return $ imageAns (answer a1) (imageOutput a1 `oplus` imageOutput a2)


-- | 'annotate' : @ image * (result -> graphic) -> Image @
-- 
-- | Annotate an image by super-imposing a graphic on top of it - 
-- the annotation function has access to the /result/ of the Image
-- before it is super-imposed.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > annotate :: Image t u -> ((t u) -> Graphic u) -> Image t u
-- > annotate :: LocImage t u -> ((t u) -> LocGraphic u) -> LocImage t u
-- > annotate :: LocThetaImage t u -> ((t u) -> LocThetaGraphic u) -> LocTheteImage t u
--
annotate :: Monad m 
         => m (ImageAns t u) -> ((t u) -> m (ImageAns t0 u0)) -> m (ImageAns t u)
annotate img f = 
    img >>= \a1 -> f (answer a1) >>= \a2 -> 
    return $ imageAns (answer a1) (imageOutput a1 `oplus` imageOutput a2)




-}