

module ImportAll where

import Data.Aviary
import Data.Aviary.Birds
import qualified Data.Aviary.BirdsInter as Inter


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