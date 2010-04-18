{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.BeatPattern
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Beat patterns
--
--------------------------------------------------------------------------------

module Bala.BeatPattern
  ( 
  -- * Data types
    BeatS  



  -- * subdivsion helpers
  , subdivide
  , subdivide2
  , subdivide3
  , subdivide4  

  -- * Beat patterns
  -- ** 2 beat subdivision  
  , _none2 
  , _x, x_, xx

  -- ** 3 beat subdivision
  , _none3
  , __x, _x_, _xx, x__, x_x, xx_, xxx

  -- ** 4 beat subdivision
  , _none4
  , ___x, __x_, __xx, _x__, _x_x, _xx_, _xxx
  , x___, x__x, x_x_, x_xx, xx__, xx_x, xxx_, xxxx

  -- * Evaluation

  ) where

import Data.JoinList ( JoinList, join, wrap )
import qualified Data.JoinList as JL


--------------------------------------------------------------------------------
-- Data types


type BeatS a b = (a -> b) -> (a -> b) -> a -> JoinList b


--------------------------------------------------------------------------------
-- subdivision

subdivide :: Fractional a => Int -> a -> [a]
subdivide n r = replicate n (r / (fromIntegral n))

subdivide2 :: Fractional a => a -> (a,a)
subdivide2 r = (r/2,r/2) 

subdivide3 :: Fractional a => a -> (a,a,a)
subdivide3 r = (r/3,r/3,r/3) 

subdivide4 :: Fractional a => a -> (a,a,a,a)
subdivide4 r = (r/4,r/4,r/4,r/4) 


--------------------------------------------------------------------------------
-- 2 beat subdivision

hprod :: (t -> u -> v) -> (a -> t) -> (b -> u) -> (a,b) -> v
hprod phi f g (a,b) = phi (f a) (g b)

-- Oh No! - we can abuse underscores in variable names to make 
-- beat patterns. 
_none2 :: Fractional a => BeatS a b
_none2 _ sp = hprod join sp' sp' . subdivide2 
  where sp' = wrap . sp


_x :: Fractional a => BeatS a b
_x f sp = hprod join  f' sp' . subdivide2 
  where f'  = wrap . f
        sp' = wrap . sp

x_ :: Fractional a => BeatS a b
x_ f sp = hprod join f' sp' . subdivide2
  where f'  = wrap . f
        sp' = wrap . sp


xx :: Fractional a => BeatS a b
xx f _ = hprod join f' f' . subdivide2
  where f'  = wrap . f


--------------------------------------------------------------------------------
-- 3 beat subdivision

join3 :: JoinList a -> JoinList a -> JoinList a -> JoinList a
join3 = (join .) . join

hprod3 :: (t -> u -> v -> w) -> (a -> t) -> (b -> u) -> (c -> v) -> (a,b,c) -> w
hprod3 phi f g h (a,b,c) = phi (f a) (g b) (h c) 

_none3 :: Fractional a => BeatS a b
_none3 _ sp = hprod3 join3 sp' sp' sp' . subdivide3
  where sp' = wrap . sp

__x :: Fractional a => BeatS a b
__x f sp = hprod3 join3 sp' sp' f' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp
  
_x_ :: Fractional a => BeatS a b
_x_ f sp = hprod3 join3 sp' f' sp' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp

_xx :: Fractional a => BeatS a b
_xx f sp = hprod3 join3 sp' f' f' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp

x__ :: Fractional a => BeatS a b
x__ f sp = hprod3 join3 f' sp' sp' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp

x_x :: Fractional a => BeatS a b
x_x f sp = hprod3 join3 f' sp' f' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp

xx_ :: Fractional a => BeatS a b
xx_ f sp = hprod3 join3 f' f' sp' . subdivide3
  where f'  = wrap . f
        sp' = wrap . sp

xxx :: Fractional a => BeatS a b
xxx f _ = hprod3 join3 f' f' f' . subdivide3
  where f'  = wrap . f


--------------------------------------------------------------------------------
-- 4 beat subdivision

join4 :: JoinList a -> JoinList a -> JoinList a -> JoinList a -> JoinList a
join4 = (((join .) .) join .) . join


hprod4 :: (t -> u -> v -> w -> x) 
       -> (a -> t) 
       -> (b -> u) 
       -> (c -> v) 
       -> (d -> w) 
       -> (a,b,c,d) 
       -> x
hprod4 phi f g h i (a,b,c,d) = phi (f a) (g b) (h c) (i d)

_none4 :: Fractional a => BeatS a b
_none4 _ sp = hprod4 join4 sp' sp' sp' sp' . subdivide4
  where sp' = wrap . sp

___x :: Fractional a => BeatS a b
___x f sp = hprod4 join4 sp' sp' sp' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

__x_ :: Fractional a => BeatS a b
__x_ f sp = hprod4 join4 sp' sp' f' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp


__xx :: Fractional a => BeatS a b
__xx f sp = hprod4 join4 sp' sp' f' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp


_x__ :: Fractional a => BeatS a b
_x__ f sp = hprod4 join4 sp' f' sp' sp' . subdivide4 
  where f'  = wrap . f
        sp' = wrap . sp


_x_x :: Fractional a => BeatS a b
_x_x f sp = hprod4 join4 sp' f' sp' f'  . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp


_xx_ :: Fractional a => BeatS a b
_xx_ f sp = hprod4 join4 sp' f' f' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

_xxx :: Fractional a => BeatS a b
_xxx f sp = hprod4 join4 sp' f' f' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

x___ :: Fractional a => BeatS a b
x___ f sp = hprod4 join4 f' sp' sp' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

x__x :: Fractional a => BeatS a b
x__x f sp = hprod4 join4 f' sp' sp' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp


x_x_ :: Fractional a => BeatS a b
x_x_ f sp = hprod4 join4 f' sp' f' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

x_xx :: Fractional a => BeatS a b
x_xx f sp = hprod4 join4 f' sp' f' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

xx__ :: Fractional a => BeatS a b
xx__ f sp = hprod4 join4 f' f' sp' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

xx_x :: Fractional a => BeatS a b
xx_x f sp = hprod4 join4 f' f' sp' f' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

xxx_ :: Fractional a => BeatS a b
xxx_ f sp = hprod4 join4 f' f' f' sp' . subdivide4
  where f'  = wrap . f
        sp' = wrap . sp

xxxx :: Fractional a => BeatS a b
xxxx f _ = hprod4 join4 f' f' f' f' . subdivide4
  where f' = wrap . f










