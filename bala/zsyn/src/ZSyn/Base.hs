{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.Base
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


module ZSyn.Base
  (
    Aud44
  , Ctl44
   
  , AudioStream
  , ControlStream 

  , Time
  , timeToSamples
  
  , lineSeg

  , upSample
  , coerce


  ) where

import ZSyn.HSStream

import Prelude hiding ( drop, map, iterate, repeat )

data Aud44

data Ctl44   -- 4410


type AudioStream   = HSStream Aud44 Double

type ControlStream = HSStream Ctl44 Double


class Clock a where
  rate :: a -> Double

instance Clock Aud44 where
  rate _ = 44100

instance Clock Ctl44 where
  rate _ = 4410


type Time = Double

timeToSamples :: Double -> Time -> Int
timeToSamples sr dur = truncate (dur * sr)


upSample :: forall p1 p2 a. (Clock p1, Clock p2)
         => HSStream p1 a -> HSStream p2 a
upSample = duplicate ratio
  where
    inn_rate = floor $ rate (undefined :: p1)
    out_rate = floor $ rate (undefined :: p2)
    ratio    = out_rate `div` inn_rate
    
coerce :: forall p1 p2 a. (Clock p1, Clock p2, RealFrac a)
       => HSStream p1 a -> HSStream p2 a
coerce ss = if ratio >= 1.0 then interpSkip ratio ss else interpUp ratio ss
  where
    inn_rate = rate (undefined :: p1)
    out_rate = rate (undefined :: p2)
    ratio    = realToFrac $ out_rate / inn_rate



lineSeg :: forall p. Clock p => [Double] -> [Time] -> HSStream p Double
lineSeg amps durs = go amps durs
  where
    sr                   = rate (undefined :: p) 

    go (a1:a2:as) (d:ds) = prefix samples ss (go (a2:as) ds)
      where
        samples          = truncate (d * sr)
        dd               = (a2 - a1) / (d * sr)
        ss               = iterate (+dd) a1

    go _          _      = repeat 0

-- ratio is [0..1]
interpUp :: RealFrac a => a -> HSStream p1 a -> HSStream p2 a
interpUp ratio ss = go (design ratio) ss
  where
    interpl d a b                 = a + (d * (b - a))

    go ((n,r) :< rs)    (_ :< xs)  | n > 0 = go ((n-1,r) :< rs) xs
    go ((_,r) :< rs) xs@(x0 :< x1 :< _ )   = interpl r x0 x1 :< go rs xs




interpSkip :: RealFrac a => a -> HSStream p1 a -> HSStream p2 a
interpSkip ratio ss = go (design ratio) ss
  where
    interpl d a b                 = a + (d * (b - a))

    go ((n,r) :< rs) xs = let (y0 :< y1 :< ys) = drop (n-1) xs
                              a                = interpl r y0 y1      
                          in a :< go rs (y1 :< ys)



design :: RealFrac a => a -> HSStream p (Int,a)
design ratio = pairs
  where
    pairs = (0,0) :< map (\((_::Int),r) -> properFraction (r + ratio)) pairs

