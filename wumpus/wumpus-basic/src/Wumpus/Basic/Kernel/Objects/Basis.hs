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

  , uconvImageF
  , uconvImageZ

  , emptyImage


  , both

  , Answer
  , UnaryObj(..)
  , BinaryObj(..)

  , MoveStart(..)

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
   


-- Note - maybe this should just be an operator on LocImage...
--
class MoveStart img where
  moveStart :: u ~ DUnit img => Vec2 u -> img -> img


-- | Note - the kind of f allows fo unit annotation.
--
ignoreAns :: Functor (f u) => f u a -> f u (UNil u)
ignoreAns = fmap (const UNil)

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor (f u) => a -> f u z -> f u a
replaceAns a = fmap (const a)


class Decorate (f :: * -> * -> *) where
  decorate :: f u a -> f u z -> f u a
  elaborate :: f u a -> (a -> f u z) -> f u a

-- | Should a decoration \"lift\" a query (Pure) to an image (PrimW)? 
--
-- Currently I don\'t think it should.
--
decoratePrimW :: PrimW u a -> PrimW u z -> PrimW u a
decoratePrimW (PrimW ca a) (PrimW cb _) = PrimW (ca `mappend` cb) a
decoratePrimW a            _            = a


-- | However elaborating seems natural to take a query (Pure) to 
-- an image (PrimW) as it works like /swapping/ bind.
--
elaborateImage :: Image u a -> (a -> Image u z) -> Image u a
elaborateImage ma k = Image $ \ ctx -> case getImage ma ctx of 
    PrimW ca a -> let cb = getCA $ getImage (k a) ctx in PrimW (ca `mappend` cb) a
    Pure a     -> let cb = getCA $ getImage (k a) ctx in PrimW cb a
  where
    getCA (PrimW ca _) = ca
    getCA _            = mempty


instance Decorate Image where
  decorate ma mz = Image $ \ctx -> 
                     decoratePrimW (getImage ma ctx) (getImage mz ctx)

  elaborate = elaborateImage  


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



-- OLD 
{-


--------------------------------------------------------------------------------



-- | Map the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
mapAns :: (a -> a1) -> ImageAns u a -> ImageAns u a1
mapAns f (Ans cp a) = Ans cp (f a) 




-- | Replace the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
replaceAns :: ans -> ImageAns u a -> ImageAns u ans
replaceAns ans (Ans prim _) = Ans prim ans


-- | Turn an imageAns into a GraphicAns by ignoring the 
-- result.
-- 
-- Usually this function will be used with one of the @push@ 
-- family of combinators.
--
-- > LocImage-to-LocGraphic = pushR1 ignoreAns 
--
-- ignoreAns :: ImageAns u a -> GraphicAns u
-- ignoreAns(Ans prim _) = Ans prim UNil



graphicAns :: CatPrim -> GraphicAns u
graphicAns prim = Ans prim UNil

-- | Extractor for the answer part of an image.
--
answer :: ImageAns u a -> a
answer (Ans _ a) = a


-- | Note - maybe this requires an arity family instead?
--
hyperlink :: XLink -> ImageAns u a -> ImageAns u a
hyperlink hypl (Ans prim a) = Ans (cpmap (xlinkPrim hypl) prim) a


-- | Clip a graphic object.
-- 
-- Note - maybe this requires an arity family instead?
--
clipObject :: PrimPath -> ImageAns t u -> ImageAns t u
clipObject pp (Ans prim a) =  Ans (cpmap (clip pp) prim) a


--------------------------------------------------------------------------------
-- Helpers for unit conversion...



szconvAnsF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
                => FontSize -> ImageAns u (t u) -> ImageAns u1 (t u1)
szconvAnsF sz (Ans prim a) = Ans prim (uconvertF sz a)

szconvAnsZ :: FontSize -> ImageAns u a -> ImageAns u1 a
szconvAnsZ _ (Ans prim a) = Ans prim a





infixr 1 `incline`


-- | Downcast a 'LocThetaQuery' function by applying it to the 
-- supplied angle, making an arity-one Context Function (a 
-- 'LocCF'). 
-- 
incline :: LocThetaQuery u a -> Radian -> LocQuery u a
incline = apply1R2


-- | Downcast a LocThetaQuery function by applying it to the 
-- supplied point and angle, making an arity-zero Context Function 
-- (a CF). 
--
atIncline :: LocThetaQuery u a -> Point2 u -> Radian -> CF a
atIncline = apply2R2



-- | Replace the ans - arity 0.
-- 
replaceAnsR0 :: ans -> CF (ImageAns u a) -> CF (ImageAns u ans)
replaceAnsR0 ans = fmap (replaceAns ans)


-- | Replace the ans - arity 1.
--
replaceAnsR1 :: ans -> CF (r1 -> ImageAns u a) -> CF (r1 -> ImageAns u ans)
replaceAnsR1 ans = fmap $ fmap (replaceAns ans)


-- | Replace the ans - arity 2.
--
replaceAnsR2 :: ans 
             -> CF (r1 -> r2 -> ImageAns u a) 
             -> CF (r1 -> r2 -> ImageAns u ans)
replaceAnsR2 ans = fmap $ fmap $ fmap (replaceAns ans) 


--
-- DESIGN NOTE
--
-- The argument orders for decorate and elaborate are 
-- uncharacteristic. Whilst the functions are essentially 
-- transformers:
-- 
-- >  a -> (b -> b)
-- 
-- ... the dataflow is better represented by the order they use:
--
-- > b -> a -> b
-- 


-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
decorateR0 :: CF (ImageAns u a) -> CF (GraphicAns u) -> CF (ImageAns u a) 
decorateR0 img gf = op <$> img <*> gf
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an arity 1 Image by superimposing a arity 1 Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types. The usual case is to decorate
-- a LocImage with a LocGraphic.
--
decorateR1 :: CF (r1 -> ImageAns u a) 
           -> CF (r1 -> GraphicAns u) 
           -> CF (r1 -> ImageAns u a) 
decorateR1 img gf = promoteR1 $ \r1 ->
    op <$> apply1R1 img r1 <*> apply1R1 gf r1
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an arity 2 Image by superimposing a arity 2 Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types. 
--
decorateR2 :: CF (r1 -> r2 -> ImageAns u a) 
           -> CF (r1 -> r2 -> GraphicAns u) 
           -> CF (r1 -> r2 -> ImageAns u a) 
decorateR2 img gf = promoteR2 $ \r1 r2 ->
    op <$> apply2R2 img r1 r2 <*> apply2R2 gf r1 r2
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
elaborateR0 :: CF (ImageAns u a) -> (a -> CF (GraphicAns u)) -> CF (ImageAns u a) 
elaborateR0 img gf = 
    img  >>= \(Ans p1 a) ->
    gf a >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a




-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
elaborateR1 :: CF (r1 -> ImageAns u a) 
            -> (a -> CF (r1 -> GraphicAns u)) 
            -> CF (r1 -> ImageAns u a) 
elaborateR1 img gf = promoteR1 $ \r1 -> 
    apply1R1 img r1    >>= \(Ans p1 a) ->
    apply1R1 (gf a) r1 >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a



elaborateR2 :: CF (r1 -> r2 -> ImageAns u a) 
            -> (a -> CF (r1 -> r2 -> GraphicAns u)) 
            -> CF (r1 -> r2 -> ImageAns u a) 
elaborateR2 img gf = promoteR2 $ \r1 r2 -> 
    apply2R2 img r1 r2    >>= \(Ans p1 a) ->
    apply2R2 (gf a) r1 r2 >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a

-}

