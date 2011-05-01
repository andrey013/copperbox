{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.Active
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes.
--
--------------------------------------------------------------------------------


module ZSyn.Active
  (

    Active
  , runActive

  , decorate
  , delayOnset
  , at

  , oplus
  , over
  
  , simpleNote
  , sawNote
  , triNote
  , squNote

  ) where

import ZSyn.Base
import ZSyn.HSStream
import ZSyn.Seconds

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Word


type Duration = Seconds
type Onset    = Seconds


data Active p a = Active Onset Duration (Duration -> HSStream p a)

instance Functor (Active p) where
  fmap f (Active on drn g) = Active on drn (\d -> fmap f (g d))

instance Applicative (Active p) where
  pure a = Active 0 0 (\_ -> pure a)

  (Active on0 d0 f) <*> (Active on1 d1 g) = 
     Active (min on0 on1) (max d0 d1) (\d -> (f d) <*> (g d))



runActive :: forall a p. Clock p => Active p Double ->  HSStream p Double
runActive (Active on d1 f) = prefix samps (pure 0) (f d1)
  where
    sr    = rate (undefined :: p)
    samps = secondsToSamples sr on 

decorate :: (HSStream p a -> HSStream p a) -> Active p a -> Active p a
decorate fn (Active on dur f) = Active on dur (\d -> (fn . f) d)

delayOnset :: Duration -> Active p a -> Active p a
delayOnset dx (Active on dur f) = Active (on + dx) dur f

infixr 6 `oplus`

at :: Duration -> Active p a -> Active p a
at x (Active _ drn f) = Active x drn f



-- | Play one Active after the other.
--
oplus :: forall a p. Clock p 
      => Active p a -> Active p a -> Active p a
oplus (Active on1 d1 f) (Active _ d2 g) =
    Active on1 tot (\d -> let dl    = d1r * d
                              dr    = d2r * d
                              samps = secondsToSamples sr dl
                          in prefix samps (f dl) (g dr))
  where
    sr  = rate (undefined :: p)
    
    tot = d1+d2

    d1r = d1/tot
    d2r = d2/tot

infixr 5 `over`

-- | Play simultaneously (additive synthesis).
--
over :: Num a => Active p a -> Active p a -> Active p a
over (Active on1 d1 f) (Active _ d2 g) =
    Active on1 (max d1 d2) (\d -> f d + g d)

simpleNote :: Double -> Duration -> Active Aud44 Double
simpleNote freq dur = Active 0 dur mk 
  where
    mk = \d -> let env = envelope 0.05 0.1 0.5 1.0 0.5 0.4 d
               in (*) (oscil freq) env



dpiSR :: Double -> Double
dpiSR sr = 2*pi / sr

oscil :: Double -> AudioStream
oscil fr = y 
  where
    ar  :: Aud44
    ar  = undefined
    ohm = dpiSR (rate ar) * fr
    y   = 0 :< v
    v   = (sin ohm) :< ((2 * cos ohm) *^ v - y)


mkNote :: (Double -> HSStream Aud44 Double) 
       -> (Double -> Duration -> Active Aud44 Double)
mkNote genWave freq dur = Active 0 dur mk 
  where
    mk = \d -> let env = envelope 0.05 0.1 0.5 1.0 0.5 0.4 d
               in (*) (genWave freq) env


sawNote :: Double -> Duration -> Active Aud44 Double
sawNote = mkNote sawtooth

triNote :: Double -> Duration -> Active Aud44 Double
triNote = mkNote triangle

squNote :: Double -> Duration -> Active Aud44 Double
squNote = mkNote square
