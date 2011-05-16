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
  , szconvPrimF
  , szconvPrimZ


  , Image
  , Graphic 
  , Query

  , DImage
  , DGraphic

  , runImage

  , primGraphic


  , UConvert(..)
  , uconvImageF
  , uconvImageZ

  , emptyImage


  , both

  , Answer
  , UnaryObj(..)
  , BinaryObj(..)

  , ignoreAns
  , replaceAns

  , Decorate(..)


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
               | Pure a

type instance DUnit (PrimW u a) = u


instance Monoid a => Monoid (PrimW u a) where
  mempty = Pure mempty
  Pure a     `mappend` Pure b     = Pure (a `mappend` b)
  Pure a     `mappend` PrimW cb b = PrimW cb (a `mappend` b)
  PrimW ca a `mappend` Pure b     = PrimW ca (a `mappend` b)
  PrimW ca a `mappend` PrimW cb b = PrimW (ca `mappend` cb) (a `mappend` b)

instance Functor (PrimW u) where
  fmap f (PrimW w a) = PrimW w (f a)
  fmap f (Pure a)    = Pure (f a)


instance Applicative (PrimW u) where
  pure a                        = Pure a
  (Pure f)     <*> ma           = fmap f ma
  (PrimW c1 f) <*> (Pure a)     = PrimW c1 (f a)
  (PrimW c1 f) <*> (PrimW c2 a) = PrimW (c1 `mappend` c2) (f a) 


instance Monad (PrimW u) where
  return a            = Pure a
  (Pure a)     >>= mf = mf a
  (PrimW c1 a) >>= mf = case mf a of
                          Pure b     -> PrimW c1 b
                          PrimW c2 b -> PrimW (c1 `mappend` c2) b


primAnswer :: PrimW u a -> a
primAnswer (PrimW _ a) = a
primAnswer (Pure a)    = a


-- | For the moment the second fun is type preserving...
--
bimapPrimW :: (CatPrim -> CatPrim) -> (a -> a) -> PrimW u a -> PrimW u a
bimapPrimW f g (PrimW ca a) = PrimW (f ca) (g a)
bimapPrimW _ g (Pure a)     = Pure (g a)




-- | Convert a PrimW where the answer is some functor type 
-- parametrized by the unit.
--
szconvPrimF :: (Functor t, InterpretUnit u, InterpretUnit u1)
            => FontSize -> PrimW u (t u)  -> PrimW u1 (t u1)
szconvPrimF sz (Pure a)    = Pure (uconvertF sz a)
szconvPrimF sz (PrimW c a) = PrimW c (uconvertF sz a)


-- | Convert a PrimW where the answer is oblivious to unit.
--
szconvPrimZ :: (InterpretUnit u, InterpretUnit u1)
            => FontSize -> PrimW u a  -> PrimW u1 a
szconvPrimZ _ (Pure a)    = Pure a
szconvPrimZ _ (PrimW c a) = PrimW c a


--------------------------------------------------------------------------------


newtype Image u a = Image { 
          getImage :: DrawingContext -> PrimW u a }

type instance DUnit (Image u a) = u

type Graphic u = Image u (UNil u)

type Query u a = Image u a


-- | Type specialized version of 'Image'.
--
type DImage a       = Image Double a

-- | Type specialized version of 'Graphic'.
--
type DGraphic       = Graphic Double 


instance Functor (Image u) where
  fmap f ma = Image $ \ctx -> fmap f $ getImage ma ctx

instance Applicative (Image u) where
  pure a    = Image $ \_ -> pure a
  mf <*> ma = Image $ \ctx -> 
                getImage mf ctx <*> getImage ma ctx


instance Monad (Image u) where
  return a = Image $ \_ -> return a
  ma >>= k = Image $ \ctx -> getImage ma ctx >>= \ans -> getImage (k ans) ctx



instance Monoid a => Monoid (Image u a) where
  mempty          = pure mempty
  ma `mappend` mb = Image $ \ctx -> 
                      getImage ma ctx `mappend` getImage mb ctx

instance DrawingCtxM (Image u) where
  askDC           = Image $ \ctx -> Pure ctx
  asksDC fn       = Image $ \ctx -> Pure (fn ctx)
  localize upd ma = Image $ \ctx -> getImage ma (upd ctx)


runImage :: DrawingContext -> Image u a -> PrimW u a
runImage ctx mf = getImage mf ctx

-- | Constructor for Primtive graphics.
--
primGraphic :: CatPrim -> Graphic u
primGraphic ca = Image $ \_ -> PrimW ca UNil



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


both :: Applicative f => f a -> f b -> f (a,b)
both fa fb = (,) <$> fa <*> fb

type family Answer a :: *

class UnaryObj (obj :: * ) where
   type UnaryR obj :: *
   promoteU :: (u ~ DUnit obj, a ~ Answer obj) 
            => (UnaryR obj -> Image u a) -> obj

   applyU   :: (u ~ DUnit obj, a ~ Answer obj) 
            => obj -> UnaryR obj -> Image u a
   


class BinaryObj (obj :: * ) where
   type BinaryR1 obj :: *
   type BinaryR2 obj :: *

   promoteB :: (u ~ DUnit obj, a ~ Answer obj, u ~ DUnit a) 
            => (BinaryR1 obj -> BinaryR2 obj -> Image u a) -> obj

   applyB   :: (u ~ DUnit obj, a ~ Answer obj, u ~ DUnit a) 
            => obj -> BinaryR1 obj -> BinaryR2 obj -> Image u a
   



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
  decorate   :: f u a -> f u z -> f u a
  elaborate  :: f u a -> (a -> f u z) -> f u a
  obliterate :: f u a -> f u z -> f u a
  hyperlink  :: XLink -> f u a -> f u a


-- | Do not export...
--
getCatPrim :: PrimW u a -> CatPrim
getCatPrim (PrimW ca _) = ca
getCatPrim (Pure _)     = mempty

-- | Should a decoration \"lift\" a query (Pure) to an image (PrimW)? 
--
-- Currently I don\'t think it should.
--
decorateImage :: Image u a -> Image u z -> Image u a
decorateImage ma mb = Image $ \ctx -> 
    step (getImage ma ctx) (getImage mb ctx)
  where
    step (PrimW ca a) (PrimW cb _) = PrimW (ca `mappend` cb) a
    step a            _            = a


-- | However elaborating seems natural to take a query (Pure) to 
-- an image (PrimW) as it works like /swapping/ bind.
--
elaborateImage :: Image u a -> (a -> Image u z) -> Image u a
elaborateImage ma k = Image $ \ ctx -> case getImage ma ctx of 
    PrimW ca a -> let cb = getCatPrim $ getImage (k a) ctx 
                  in PrimW (ca `mappend` cb) a
    Pure a     -> let cb = getCatPrim $ getImage (k a) ctx in PrimW cb a

obliterateImage :: Image u a -> Image u z -> Image u a
obliterateImage ma mb = Image $ \ctx -> 
    let a  = primAnswer $ getImage ma ctx
        ca = getCatPrim $ getImage mb ctx
    in PrimW ca a
  
hyperlinkImage :: XLink -> Image u a -> Image u a
hyperlinkImage xl ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (PrimW ca a) = PrimW (cpmap (xlinkPrim xl) ca) a
    step (Pure a)     = Pure a



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
  rotate ang (Pure a)     = Pure  (rotate ang a)


instance (RotateAbout a, ScalarUnit u, u ~ DUnit a) => 
    RotateAbout (PrimW u a) where
  
  rotateAbout ang pt@(P2 x y) (PrimW ca a) = 
    PrimW (rotateAbout ang (P2 (toPsPoint x) (toPsPoint y)) ca)
          (rotateAbout ang pt a) 
        
  rotateAbout ang pt (Pure a) = Pure (rotateAbout ang pt a) 


instance Scale a => Scale (PrimW u a) where
  scale sx sy (PrimW ca a) = PrimW (scale sx sy ca) (scale sx sy a)
  scale sx sy (Pure a)     = Pure (scale sx sy a)


instance (Translate a, ScalarUnit u, u ~ DUnit a) => 
    Translate (PrimW u a) where

  translate dx dy (PrimW cp a) = 
    PrimW (translate (toPsPoint dx) (toPsPoint dy) cp) (translate dx dy a) 

  translate dx dy (Pure a) = Pure (translate dx dy a) 


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

-- | Clip a graphic object.
-- 
-- Note - maybe this requires an arity family instead?
--
clipObject :: PrimPath -> ImageAns t u -> ImageAns t u
clipObject pp (Ans prim a) =  Ans (cpmap (clip pp) prim) a





-- | Downcast a LocThetaQuery function by applying it to the 
-- supplied point and angle, making an arity-zero Context Function 
-- (a CF). 
--
atIncline :: LocThetaQuery u a -> Point2 u -> Radian -> CF a
atIncline = apply2R2



-}

