{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.SpecMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low-level abstract syntax
--
--------------------------------------------------------------------------------

module Sound.FMSS.SpecMonad
  (

    Spec
  , execSpec

  , modulator
  , carrier

  , (=>-)
  , (=>=)

  ) where


import Sound.FMSS.Datatypes
import Sound.FMSS.Utils.HList

import Control.Applicative
import Data.Monoid


-- Note - potentially this can be implemented as an inner
-- /connection spec/. With sharing for envelopes in an outer
-- monad.


data St = St { mod_count :: !Int, car_count :: Int }

data W = W { w_mods :: H Modulator, w_cars :: H Carrier, w_links :: H Link }

instance Monoid W where
  mempty = W emptyH emptyH emptyH
  W a1 b1 c1 `mappend` W a2 b2 c2 = 
      W (a1 `appendH` a2) (b1 `appendH` b2) (c1 `appendH` c2)

newtype Spec a = Spec { getSpec :: St -> (a, St, W) }

instance Functor Spec where
  fmap f mf = Spec $ \s -> let (a,s1,w) = getSpec mf s in (f a, s1, w)

instance Applicative Spec where
  pure a    = Spec $ \s -> (a,s,mempty)
  mf <*> ma = Spec $ \s -> let (f,s1,w1) = getSpec mf s
                               (a,s2,w2) = getSpec ma s1
                           in (f a, s2, w1 `mappend` w2) 

instance Monad Spec where
  return a  = Spec $ \s -> (a,s,mempty)
  ma >>= k  = Spec $ \s -> let (a,s1,w1) = getSpec ma s
                               (b,s2,w2) = getSpec (k a) s1
                           in (b, s2, w1 `mappend` w2)

execSpec :: Spec a -> SynthBody
execSpec mf = post $ getSpec mf s0
  where
    post (_,_,w) = SynthBody { synth_mods  = toListH $ w_mods w
                             , synth_cars  = toListH $ w_cars w
                             , synth_links = toListH $ w_links w
                             }

    s0 = St { mod_count = 1, car_count = 1}

modulator :: Oscil -> Spec Modulator
modulator osc = Spec $ \s@(St {mod_count=ix}) ->
    let m1 = Modulator ix osc
    in (m1, s { mod_count = ix+1}, mempty { w_mods = wrapH m1})
                         

carrier :: Oscil -> Spec Carrier
carrier osc = Spec $ \s@(St {car_count=ix}) ->
    let c1 = Carrier ix osc
    in (c1, s { mod_count = ix+1}, mempty { w_cars = wrapH c1})

(=>-) :: Modulator -> Carrier -> Spec ()
(=>-) m1 c1 = Spec $ \s -> 
    let link = ModCar (oscilNum m1) (oscilNum c1) 
    in ((),s, mempty { w_links = wrapH link })

(=>=) :: Modulator -> Modulator -> Spec ()
(=>=) m1 m2 = Spec $ \s -> 
    let link = ModMod (oscilNum m1) (oscilNum m2) 
    in ((),s, mempty { w_links = wrapH link })
