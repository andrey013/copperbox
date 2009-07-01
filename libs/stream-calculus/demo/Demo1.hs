

module Demo1 where

import Data.Stream
import Data.Stream.StreamCalculus

import Prelude hiding ( take, const, zipWith, repeat, map, tail )

equivUpto n s z = and $ take n $ zipWith (==) s z

fcounting :: Fractional a => Stream a
fcounting = repeat 1 * repeat 1

nats' = 1 <: map (+1) nats' 

oneOone = 1 <: 0 <: oneOone


d01 = take 10 $ blam (const 4)
d01'1 = take 10 $ blam (cX)

n10 = equivUpto 10 (const 4 * s) (s * const 4)  where s = fcounting 
n11 = equivUpto 10 (prime $ cX * s) s           where s = oneOone
n12 = equivUpto 10 (s * cX) (cX * s)            where s = nats'


n13 = equivUpto 10 (prime (powX 4)) (powX 3)

n14 = equivUpto 10 (s * (inverse s)) 1          where s = const 4
n15 = equivUpto 10 ((inverse s) * s) 1          where s = fcounting

n16 = equivUpto 10 (inverse (inverse s)) s      where s = const 10

n17 = equivUpto 10 (inverse (s * t)) ((inverse s) * (inverse t) ) 
  where s = fcounting
        t = repeat 1

n18 = equivUpto 10 (inverse $ 1 - cX) (polynomial (replicate 10 1))

n19 = equivUpto 10 (cX * s) (0 <: s)            where s = oneOone


-- n25 = 

n26 = equivUpto 10 (s <.> cX) s                 where s = nats'
n26'1 = take 10 (s <.> negate cX)               where s = nats'


n28 = equivUpto 10 s (sroot s * sroot s)        where s = fcounting


n30 = equivUpto 10 (sroot s * sroot t) (sroot $ s * t) 
  where s = fcounting; t = repeat 4 

n61 = equivUpto 10 (deriv s) (prime (cX <#> (prime s))) 
  where s = fcounting

n63 = equivUpto 10 (deriv $ s * t) (((deriv s) * t) + (s * (deriv t)))
  where s = fcounting
        t = fcounting


n64 = equivUpto 10 (deriv $ inverse s) (-(deriv s) * (inverse s) * (inverse s))
  where s = fcounting