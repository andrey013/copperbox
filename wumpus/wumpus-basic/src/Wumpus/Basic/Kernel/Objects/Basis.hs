{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Common types and operations.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Basis
  (

    PrimW(..)
  , primAnswer

  , Image
  , Graphic 

  , Query

  , DImage
  , DGraphic

  , runImage
  , runQuery
  , zapQuery

  , primGraphic
  , clipImage

  , UConvert(..)
  , uconvImageF
  , uconvImageZ

  , emptyImage

  , ignoreAns
  , replaceAns

  , Decorate(..)
  , sdecorate
  , adecorate

  , selaborate
  , aelaborate

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Monoid



-- | Unit @u@ is a phantom.
--
data PrimW u a = PrimW CatPrim a

type instance DUnit (PrimW u a) = u


instance Monoid a => Monoid (PrimW u a) where
  mempty = PrimW mempty mempty
  PrimW ca a `mappend` PrimW cb b = PrimW (ca `mappend` cb) (a `mappend` b)

instance Functor (PrimW u) where
  fmap f (PrimW w a) = PrimW w (f a)


instance Applicative (PrimW u) where
  pure a                        = PrimW mempty a
  (PrimW c1 f) <*> (PrimW c2 a) = PrimW (c1 `mappend` c2) (f a) 


instance Monad (PrimW u) where
  return a            = PrimW mempty a
  (PrimW c1 a) >>= mf = let (PrimW c2 b) = mf a
                        in PrimW (c1 `mappend` c2) b


primAnswer :: PrimW u a -> a
primAnswer (PrimW _ a) = a


-- | For the moment the second fun is type preserving...
--
bimapPrimW :: (CatPrim -> CatPrim) -> (a -> a) -> PrimW u a -> PrimW u a
bimapPrimW f g (PrimW ca a) = PrimW (f ca) (g a)







-- | Convert a PrimW where the answer is some functor type 
-- parametrized by the unit.
--
szconvPrimF :: (Functor t, InterpretUnit u, InterpretUnit u1)
            => FontSize -> PrimW u (t u)  -> PrimW u1 (t u1)
szconvPrimF sz (PrimW c a) = PrimW c (uconvertF sz a)


-- | Convert a PrimW where the answer is oblivious to unit.
--
szconvPrimZ :: (InterpretUnit u, InterpretUnit u1)
            => FontSize -> PrimW u a  -> PrimW u1 a
szconvPrimZ _ (PrimW c a) = PrimW c a


--------------------------------------------------------------------------------


newtype Image u a = Image { 
          getImage :: DrawingContext -> PrimW u a }

type instance DUnit (Image u a) = u

type Graphic u = Image u (UNil u)


-- | Type specialized version of 'Image'.
--
type DImage a       = Image Double a

-- | Type specialized version of 'Graphic'.
--
type DGraphic       = Graphic Double 


newtype Query u a = Query { 
          getQuery :: DrawingContext -> a }

type instance DUnit (Query u a) = u

-- Functor

instance Functor (Image u) where
  fmap f ma = Image $ \ctx -> fmap f $ getImage ma ctx

instance Functor (Query u) where
  fmap f ma = Query $ \ctx -> f $ getQuery ma ctx

-- Applicative

instance Applicative (Image u) where
  pure a    = Image $ \_   -> pure a
  mf <*> ma = Image $ \ctx -> 
                getImage mf ctx <*> getImage ma ctx

instance Applicative (Query u) where
  pure a    = Query $ \_   -> a
  mf <*> ma = Query $ \ctx -> let f = getQuery mf ctx 
                                  a = getQuery ma ctx
                              in f a


-- Monad

instance Monad (Image u) where
  return a = Image $ \_ -> return a
  ma >>= k = Image $ \ctx -> getImage ma ctx >>= \ans -> getImage (k ans) ctx


instance Monad (Query u) where
  return a = Query $ \_   -> a
  ma >>= k = Query $ \ctx -> let a = getQuery ma ctx in getQuery (k a) ctx

-- Monoid

instance Monoid a => Monoid (Image u a) where
  mempty          = pure mempty
  ma `mappend` mb = Image $ \ctx -> 
                      getImage ma ctx `mappend` getImage mb ctx

instance Monoid a => Monoid (Query u a) where
  mempty          = pure mempty
  ma `mappend` mb = Query $ \ctx -> 
                      getQuery ma ctx `mappend` getQuery mb ctx


-- DrawingCtxM


instance DrawingCtxM (Image u) where
  askDC           = Image $ \ctx -> return ctx
  asksDC fn       = Image $ \ctx -> return (fn ctx)
  localize upd ma = Image $ \ctx -> getImage ma (upd ctx)

instance DrawingCtxM (Query u) where
  askDC           = Query $ \ctx -> ctx
  asksDC fn       = Query $ \ctx -> (fn ctx)
  localize upd ma = Query $ \ctx -> getQuery ma (upd ctx)


runImage :: DrawingContext -> Image u a -> PrimW u a
runImage ctx mf = getImage mf ctx

runQuery :: DrawingContext -> Query u a -> a
runQuery ctx mf = getQuery mf ctx



zapQuery :: Query u a -> Image u a
zapQuery mq = askDC >>= \ctx -> let a = runQuery ctx mq in return a

-- | Constructor for Primtive graphics.
--
primGraphic :: CatPrim -> Graphic u
primGraphic ca = Image $ \_ -> PrimW ca UNil


-- | Clip an Image.
-- 
clipImage :: PrimPath -> Image u a -> Image u a
clipImage pp ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (PrimW ca a) = PrimW (cpmap (clip pp) ca) a



class UConvert (f :: * -> * -> *) where
  uconvF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
         => f u (t u) -> f u1 (t u1)

  uconvZ :: (InterpretUnit u, InterpretUnit u1) 
         => f u a -> f u1 a

instance UConvert Image where
  uconvZ = uconvImageZ
  uconvF = uconvImageF

uconvImageF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
            => Image u (t u) -> Image u1 (t u1) 
uconvImageF ma = Image $ \ctx -> 
                   let sz  = dc_font_size ctx
                       ans = getImage ma ctx
                   in szconvPrimF sz ans


uconvImageZ :: (InterpretUnit u, InterpretUnit u1) 
            => Image u a -> Image u1 a
uconvImageZ ma = Image $ \ctx -> 
                   let sz  = dc_font_size ctx
                       ans = getImage ma ctx
                   in szconvPrimZ sz ans


-- | Having /empty/ at the specific 'Image' type is useful.
-- 
emptyImage :: Monoid a => Image u a
emptyImage = mempty

--------------------------------------------------------------------------------



-- | Note - the kind of f allows fo unit annotation.
--
ignoreAns :: Functor (f u) => f u a -> f u (UNil u)
ignoreAns = fmap (const UNil)

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor (f u) => a -> f u z -> f u a
replaceAns a = fmap (const a)




-- | Decorate an object
--
-- oliterate - drops the graphic from the first object replacing 
-- it with the graphic from the second.
--
class Decorate (f :: * -> * -> *) where
  decorate   :: ZDeco -> f u a -> f u z -> f u a
  elaborate  :: ZDeco -> f u a -> (a -> f u z) -> f u a
  obliterate :: f u a -> f u z -> f u a
  hyperlink  :: XLink -> f u a -> f u a


sdecorate :: Decorate f => f u a -> f u z -> f u a
sdecorate = decorate SUPERIOR

adecorate :: Decorate f => f u a -> f u z -> f u a
adecorate = decorate ANTERIOR


selaborate :: Decorate f => f u a -> (a -> f u z) -> f u a
selaborate = elaborate SUPERIOR

aelaborate :: Decorate f => f u a -> (a -> f u z) -> f u a
aelaborate = elaborate ANTERIOR




-- | Do not export...
--
getCatPrim :: PrimW u a -> CatPrim
getCatPrim (PrimW ca _) = ca

-- | Decorate Image.
--
decorateImage :: ZDeco -> Image u a -> Image u z -> Image u a
decorateImage zo ma mb = Image $ \ctx -> 
    step zo (getImage ma ctx) (getImage mb ctx)
  where
    step SUPERIOR (PrimW ca a) (PrimW cb _) = PrimW (ca `mappend` cb) a
    step ANTERIOR (PrimW ca a) (PrimW cb _) = PrimW (cb `mappend` ca) a


-- | Elaborate Image.
--
elaborateImage :: ZDeco -> Image u a -> (a -> Image u z) -> Image u a
elaborateImage zo ma k = Image $ \ ctx ->
    let (PrimW ca a) = getImage ma ctx
        (PrimW cb _) = getImage (k a) ctx 
    in case zo of
      SUPERIOR -> PrimW (ca `mappend` cb) a
      ANTERIOR -> PrimW (cb `mappend` ca) a


obliterateImage :: Image u a -> Image u z -> Image u a
obliterateImage ma mb = Image $ \ctx -> 
    let a  = primAnswer $ getImage ma ctx
        ca = getCatPrim $ getImage mb ctx
    in PrimW ca a
  
hyperlinkImage :: XLink -> Image u a -> Image u a
hyperlinkImage xl ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (PrimW ca a) = PrimW (cpmap (xlinkPrim xl) ca) a



instance Decorate Image where
  decorate    = decorateImage
  elaborate   = elaborateImage  
  obliterate  = obliterateImage
  hyperlink   = hyperlinkImage
  
--------------------------------------------------------------------------------
-- Affine instances 

-- 
-- Design Note
--
-- Are PrimW instances needed as Image cannot use them?
-- 

instance Rotate a => Rotate (PrimW u a) where
  rotate ang (PrimW ca a) = PrimW (rotate ang ca) (rotate ang a)


instance (RotateAbout a, ScalarUnit u, u ~ DUnit a) => 
    RotateAbout (PrimW u a) where
  rotateAbout ang pt@(P2 x y) (PrimW ca a) = 
    PrimW (rotateAbout ang (P2 (toPsPoint x) (toPsPoint y)) ca)
          (rotateAbout ang pt a) 
        


instance Scale a => Scale (PrimW u a) where
  scale sx sy (PrimW ca a) = PrimW (scale sx sy ca) (scale sx sy a)


instance (Translate a, ScalarUnit u, u ~ DUnit a) => 
    Translate (PrimW u a) where
  translate dx dy (PrimW cp a) = 
    PrimW (translate (toPsPoint dx) (toPsPoint dy) cp) (translate dx dy a) 



-- Image
-- Cannot use /fmap/ as it does not touch the CatPrim (w)


instance Rotate a => Rotate (Image u a) where
  rotate ang ma = Image $ \ctx -> 
      bimapPrimW (rotate ang) (rotate ang) $ getImage ma ctx


instance (RotateAbout a, InterpretUnit u, u ~ DUnit a) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt ma = Image $ \ctx -> 
      let ptu = uconvertF (dc_font_size ctx) pt
      in bimapPrimW (rotateAbout ang ptu) (rotateAbout ang pt) $ getImage ma ctx


instance Scale a => Scale (Image u a) where
  scale sx sy ma = Image $ \ctx -> 
      bimapPrimW (scale sx sy) (scale sx sy) $ getImage ma ctx

instance (Translate a, InterpretUnit u, u ~ DUnit a) => 
    Translate (Image u a) where
  translate dx dy ma = Image $ \ctx -> 
      let sz  = dc_font_size ctx
          ddx = uconvert1 sz dx
          ddy = uconvert1 sz dy
      in bimapPrimW (translate ddx ddy) (translate dx dy) $ getImage ma ctx


--------------------------------------------------------------------------------





{-

-- OLD STUFF


-- | Downcast a LocThetaQuery function by applying it to the 
-- supplied point and angle, making an arity-zero Context Function 
-- (a CF). 
--
atIncline :: LocThetaQuery u a -> Point2 u -> Radian -> CF a
atIncline = apply2R2



-}

