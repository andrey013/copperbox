

module ImportAll where

import Data.Aviary

main :: IO ()
main = putStrLn "Well, it compiles..."

bluebird' :: (b -> c) -> (a -> b) -> a -> c
bluebird' = \a b c -> a (b c)


test1 = f1 a == f2 a && f1 a == f3 a where
  f1 = bluebird  show (+1) 
  f2 = bluebird' show (+1)
  f3 = show . (+1)
  a  = 2000

-- lark doesn't type check
-- lark = \a b -> a (b b)


on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' f g = \x y -> f (g x) (g y)

psi' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi' = \x y z w -> x (y z) (y w)