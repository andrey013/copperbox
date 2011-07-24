{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.ConfigMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Configuration of carriers and modulators
--
--------------------------------------------------------------------------------

module Sound.FMSS.ConfigMonad
  (

    Config
  , runConfig

  , modulator
  , carrier

  , (=>-)
  , (=>=)

  , Out1
  , out1
  , Out2
  , out2
  , Out3
  , out3
  , Out4
  , out4
  , Out5
  , out5
  , Out6
  , out6

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Translate ( outputVar )
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

newtype Config a = Config { getConfig :: St -> (a, St, W) }

instance Functor Config where
  fmap f mf = Config $ \s -> let (a,s1,w) = getConfig mf s in (f a, s1, w)

instance Applicative Config where
  pure a    = Config $ \s -> (a,s,mempty)
  mf <*> ma = Config $ \s -> let (f,s1,w1) = getConfig mf s
                                 (a,s2,w2) = getConfig ma s1
                             in (f a, s2, w1 `mappend` w2) 

instance Monad Config where
  return a  = Config $ \s -> (a,s,mempty)
  ma >>= k  = Config $ \s -> let (a,s1,w1) = getConfig ma s
                                 (b,s2,w2) = getConfig (k a) s1
                             in (b, s2, w1 `mappend` w2)

runConfig :: Config a -> (a,SynthBody)
runConfig mf = post $ getConfig mf s0
  where
    post (a,_,w) = (a,syn) 
      where
        syn = SynthBody { synth_mods  = toListH $ w_mods w
                        , synth_cars  = toListH $ w_cars w
                        , synth_links = toListH $ w_links w
                        }

    s0 = St { mod_count = 1, car_count = 1}

modulator :: Oscil -> Config Modulator
modulator osc = Config $ \s@(St {mod_count=ix}) ->
    let m1 = Modulator ix osc
    in (m1, s { mod_count = ix+1}, mempty { w_mods = wrapH m1})
                         

carrier :: Oscil -> Config Carrier
carrier osc = Config $ \s@(St {car_count=ix}) ->
    let c1 = Carrier ix osc
    in (c1, s { mod_count = ix+1}, mempty { w_cars = wrapH c1})

(=>-) :: Modulator -> Carrier -> Config ()
(=>-) m1 c1 = Config $ \s -> 
    let link = ModCar (oscilNum m1) (oscilNum c1) 
    in ((),s, mempty { w_links = wrapH link })

(=>=) :: Modulator -> Modulator -> Config ()
(=>=) m1 m2 = Config $ \s -> 
    let link = ModMod (oscilNum m1) (oscilNum m2) 
    in ((),s, mempty { w_links = wrapH link })


--------------------------------------------------------------------------------

type Out1 = Expr

out1 :: Carrier -> Out1
out1 = outputVar


type Out2 = (Expr, Expr)

out2 :: (Carrier,Carrier) -> Out2
out2 (a,b) = (outputVar a, outputVar b)

type Out3 = (Expr, Expr, Expr)

out3 :: (Carrier, Carrier, Carrier) -> Out3
out3 (a,b,c) = (outputVar a, outputVar b, outputVar c)


type Out4 = (Expr, Expr, Expr, Expr)

out4 :: (Carrier, Carrier, Carrier, Carrier) -> Out4
out4 (a,b,c,d) = (outputVar a, outputVar b, outputVar c, outputVar d)


type Out5 = (Expr, Expr, Expr, Expr, Expr)

out5 :: (Carrier, Carrier, Carrier, Carrier, Carrier) -> Out5
out5 (a,b,c,d,e) = ( outputVar a, outputVar b, outputVar c
                   , outputVar d, outputVar e )


type Out6 = (Expr, Expr, Expr, Expr, Expr, Expr)

out6 :: (Carrier, Carrier, Carrier, Carrier, Carrier, Carrier) -> Out6
out6 (a,b,c,d,e,f) = ( outputVar a, outputVar b, outputVar c
                     , outputVar d, outputVar e, outputVar f 
                     )
