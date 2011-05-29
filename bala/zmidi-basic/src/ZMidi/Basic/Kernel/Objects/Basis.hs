{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Basis
  ( 
    PrimW(..)
  , primAnswer

  , Event
  , runEvent

  , primEvent
  , emptyEvent

  , ignoreAns
  , replaceAns

  ) where

import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Base.WrappedPrimitive

import Control.Applicative
import Data.Monoid


-- | Unit @u@ is a phantom - it is some unit of duration.
--
data PrimW u a = PrimW CatPrim a


instance Monoid a => Monoid (PrimW u a) where
  mempty = PrimW mempty mempty
  PrimW ca a `mappend` PrimW cb b = PrimW (ca `mappend` cb) (a `mappend` b)

instance Functor (PrimW  u) where
  fmap f (PrimW ca a) = PrimW ca (f a)


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

-- bimapPrimW :: (CatPrim -> CatPrim) -> (a -> a) -> PrimW u a -> PrimW u a
-- bimapPrimW f g (PrimW ca a) = PrimW (f ca) (g a)



newtype Event u a = Event { 
          getEvent :: RenderContext -> PrimW u a }



-- Functor

instance Functor (Event u) where
  fmap f ma = Event $ \ctx -> fmap f $ getEvent ma ctx



-- Applicative

instance Applicative (Event u) where
  pure a    = Event $ \_   -> pure a
  mf <*> ma = Event $ \ctx -> 
                getEvent mf ctx <*> getEvent ma ctx


-- Monad

instance Monad (Event u) where
  return a = Event $ \_   -> return a
  ma >>= k = Event $ \ctx -> getEvent ma ctx >>= \ans -> getEvent (k ans) ctx

-- Monoid

instance Monoid a => Monoid (Event u a) where
  mempty          = Event $ \_   -> mempty
  ma `mappend` mb = Event $ \ctx -> 
                      getEvent ma ctx `mappend` getEvent mb ctx


-- RenderCtxM


instance RenderCtxM (Event u) where
  askCtx          = Event $ \ctx -> return ctx
  asksCtx fn      = Event $ \ctx -> return (fn ctx)
  localize upd ma = Event $ \ctx -> getEvent ma (upd ctx)


runEvent :: RenderContext -> Event u a -> PrimW u a
runEvent ctx mf = getEvent mf ctx



-- | Constructor for Primtive graphics.
--
primEvent :: CatPrim -> Event u ()
primEvent ca = Event $ \_ -> PrimW ca ()


-- | Having /empty/ at the specific 'Event' type is useful.
-- 
emptyEvent :: Monoid a => Event u a
emptyEvent = mempty





-- | Ignore an answer changing it to @()@.
--
ignoreAns :: Functor f => f a -> f ()
ignoreAns = fmap (const ())

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor f => a -> f z -> f a
replaceAns a = fmap (const a)



{-

-- 
-- Note - decorate seems to be a lot less useful for music than it 
-- is for graphics.
-- 
-- If sequential concat is the better interpretation of mappend 
-- for CatPrim, then decorate might not be useful at all...
--



-- | Decorate an object
--
-- oliterate - drops the residual event from the first object 
-- replacing it with the one from the second but returning the
-- original /answer/.
--
class Decorate (f :: * -> * -> *) where
  decorate   :: f u a -> f u z -> f u a
  elaborate  :: f u a -> (a -> f u z) -> f u a
  obliterate :: f u a -> f u z -> f u a


-- | Decorate Event.
--
-- Note - this purely decoration and is not introspective, so it 
-- cannot model appoggiatura which would need to subtract time 
-- from the original note.
-- 
decorateEvent :: Event u a -> Event u z -> Event u a
decorateEvent ma mb = Event $ \ctx -> 
    step (getEvent ma ctx) (getEvent mb ctx)
  where
    step (PrimW ca a) (PrimW cb _) = PrimW (ca `mappend` cb) a


-- | Elaborate Image.
--
elaborateEvent :: Event u a -> (a -> Event u z) -> Event u a
elaborateEvent ma k = Event $ \ ctx ->
    let (PrimW ca a) = getEvent ma ctx
        (PrimW cb _) = getEvent (k a) ctx 
    in PrimW (ca `mappend` cb) a
      


obliterateEvent :: Event u a -> Event u z -> Event u a
obliterateEvent ma mb = Event $ \ctx -> 
    let (PrimW _  a) = getEvent ma ctx
        (PrimW cb _) = getEvent mb ctx
    in PrimW cb a


instance Decorate Event where
  decorate    = decorateEvent
  elaborate   = elaborateEvent  
  obliterate  = obliterateEvent

-}