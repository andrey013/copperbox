

module ImportAll where

import Data.Aviary
import Data.Aviary.Birds
import qualified Data.Aviary.BirdsInter as Inter
import qualified Data.Aviary.BirdsVersion as Version

-- curried prod
cprod :: (a -> c) -> (b -> d) -> a -> b -> (c,d)
cprod = appro (,)

-- | @ S (S K) @
sparensSK :: ((a -> b) -> a) -> (a -> b) -> a 
sparensSK = starling (starling kestrel)


bluebird_alt :: (b -> c) -> (a -> b) -> a -> c
bluebird_alt = \a b c -> a (b c)


test1 = f1 a == f2 a && f1 a == f3 a where
  f1 = bluebird     show (+1) 
  f2 = bluebird_alt show (+1)
  f3 = show . (+1)
  a  = 2000

-- lark doesn't type check
-- lark = \a b -> a (b b)

owl x y = y (x y)


on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' f g = \x y -> f (g x) (g y)

psi' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi' = \x y z w -> x (y z) (y w)


--- 
appro_combfi :: (c -> b -> d) -> (a -> c) -> a -> b -> d
appro_combfi comb f = appro comb f id


psi_inter :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi_inter c f x y = dovekie c f x f y

fn = bluebird bluebird idiot

f2 = cardinal bluebird idiot

f3 :: (a -> b -> d -> e) -> a -> b -> (c -> d) -> c -> e
f3 = bluebird bluebird'

f4 :: (b -> c) -> (a-> b) -> a -> c
f4 = cardinal blackbird applicator

f5 :: (a -> c -> b -> d) -> a -> b -> c -> d
f5 = bluebird cardinal

f6 :: (b -> d -> e) -> (a -> b) -> a -> (c -> d) -> c -> e
f6 = dovekie
-- f6 = bluebird dickcissel bluebird

f7 :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
f7 = appro
-- f7 = dovekie

f8 = bluebird starling appro


psi_inter' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi_inter' = cardinal (bluebird starling (bluebird cardinalstar dovekie)) applicator
  where
    b = bluebird
    s = starling
  

 