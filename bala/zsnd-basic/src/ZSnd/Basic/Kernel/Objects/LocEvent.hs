{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.LocEvent
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ...
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.LocEvent
   (


     LocEvent
   , DLocEvent

   , runLocEvent


   , promoteLoc
   , applyLoc

   , emptyLocEvent

   , moveStart
   , at


   -- * Composing LocImages
   , distrib
   , duplicate

   )

   where



import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Objects.Basis



import Control.Applicative
import Data.Monoid



-- | 'LocEvent' - function from onset time and Context to a 
-- polymorphic /answer/ and a note /primitive/ (PrimW).
--
newtype LocEvent ctx u a = LocEvent { 
          getLocEvent :: OnsetDbl -> Event ctx u a }


type instance DUnit (LocEvent ctx u a) = u
type instance Ctx (LocEvent ctx u) = ctx



-- | Type specialized version of 'LocImage'.
--
type DLocEvent a        = LocEvent Double a


-- Functor

instance Functor (LocEvent ctx u) where
  fmap f ma = LocEvent $ \pt -> fmap f $ getLocEvent ma pt


-- Applicative

instance Applicative (LocEvent ctx u) where
  pure a    = LocEvent $ \_  -> pure a
  mf <*> ma = LocEvent $ \ot -> getLocEvent mf ot <*> getLocEvent ma ot


-- Monad

instance Monad (LocEvent ctx u) where
  return a  = LocEvent $ \_  -> return a
  ma >>= k  = LocEvent $ \ot -> getLocEvent ma ot >>= \ans -> 
                                  getLocEvent (k ans) ot



-- Monoid

instance Monoid a => Monoid (LocEvent ctx u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocEvent $ \ot -> 
                      getLocEvent ma ot `mappend` getLocEvent mb ot


-- DrawingCtxM

instance ContextM (LocEvent ctx u) where
  askCtx          = LocEvent $ \_  -> askCtx
  asksCtx fn      = LocEvent $ \_  -> asksCtx fn
  localize upd ma = LocEvent $ \ot -> localize upd (getLocEvent ma ot)


  
instance Decorate LocEvent where
  decorate ma mz = LocEvent $ \ot -> 
    decorate (getLocEvent ma ot) (getLocEvent mz ot)

  elaborate ma f = LocEvent $ \ot -> 
    elaborate (getLocEvent ma ot) (\a -> getLocEvent (f a) ot)

  obliterate ma mz = LocEvent $ \ot -> 
    getLocEvent ma ot `obliterate` getLocEvent mz ot

-- Note - if we want time unit @u@ representing symbolic durations 
-- (e.g. whole notes, quarter notes etc.) it makes sense to have 
-- tempo in the Context.
-- 

runLocEvent :: (CtxTempo ctx, InterpretUnit u)
            => u -> ctx -> LocEvent ctx u a -> PrimW u a
runLocEvent start ctx mf = 
    let dzero = normalize (tempo ctx) start 
    in runEvent ctx (getLocEvent mf dzero)



promoteLoc :: (CtxTempo ctx, InterpretUnit u) 
           => (u -> Event ctx u a) -> LocEvent ctx u a
promoteLoc k = LocEvent $ \ot -> dinterpCtx ot >>= \uot -> k uot


applyLoc :: (CtxTempo ctx, InterpretUnit u) 
         => LocEvent ctx u a -> u -> Event ctx u a
applyLoc mq ot = normalizeCtx ot >>= \dot -> getLocEvent mq dot



--------------------------------------------------------------------------------


instance UConvert LocEvent where
  uconvF = uconvLocEventF
  uconvZ = uconvLocEventZ

-- | Use this to convert 'LocGraphic' or 'LocEvent' with Functor 
-- answer.
--
uconvLocEventF :: (CtxTempo ctx, InterpretUnit u, InterpretUnit u1, Functor t) 
               => LocEvent ctx u (t u) -> LocEvent ctx u1 (t u1)
uconvLocEventF ma = LocEvent $ \ot -> uconvF $ getLocEvent ma ot



-- | Use this to convert 'LocEvent' with unit-less answer.
--
uconvLocEventZ :: (CtxTempo ctx, InterpretUnit u, InterpretUnit u1) 
               => LocEvent ctx u a -> LocEvent ctx u1 a
uconvLocEventZ ma = LocEvent $ \ot -> uconvZ $ getLocEvent ma ot



-- | Having /empty/ at the specific 'LocEvent' type is useful.
-- 
emptyLocEvent :: Monoid a => LocEvent ctx u a
emptyLocEvent = mempty




-- Note - maybe this should just be an operator on LocEvent...
--

moveStart :: (CtxTempo ctx, InterpretUnit u)
          => u -> LocEvent ctx u a -> LocEvent ctx u a
moveStart dt ma = LocEvent $ \t1 -> 
    normalizeCtx dt >>= \ddt -> getLocEvent ma (t1 + ddt) 



infixr 1 `at`


-- | Downcast a 'LocEvent' function by applying it to the supplied 
-- point, making an 'Event'. 
-- 
at :: (CtxTempo ctx, InterpretUnit u) 
   => LocEvent ctx u a -> u -> Event ctx u a
at mf t1 = normalizeCtx t1 >>= \dt1 -> getLocEvent mf dt1



--------------------------------------------------------------------------------
-- Combining LocEvents 


--
-- LocEvents have no concept of /end-time/ as the are a functional 
-- type, so they can only be combined by manipulating the start 
-- time of successive events.
--



distrib :: (Monoid a, CtxTempo ctx, InterpretUnit u) 
        => u -> [LocEvent ctx u a]  -> LocEvent ctx u a
distrib _  []     = mempty
distrib dt (x:xs) = promoteLoc $ \ot -> 
    go (applyLoc x ot) (ot + dt) xs
  where
    go acc _  []     = acc
    go acc ot (a:as) = go (acc `mappend` applyLoc a ot) (ot + dt) as


-- | This is analogue to @replicate@ in the Prelude.
--
duplicate :: (Monoid a, CtxTempo ctx, InterpretUnit u) 
          => Int -> u -> LocEvent ctx u a -> LocEvent ctx u a
duplicate n _  _   | n < 1 = mempty
duplicate n dt evt         = go evt dt (n-1)
  where
     go acc _  i | i < 1 = acc
     go acc d1 i         = let evt1 = moveStart d1 evt
                           in go (acc `mappend` evt1) (d1 + dt) (i-1)

