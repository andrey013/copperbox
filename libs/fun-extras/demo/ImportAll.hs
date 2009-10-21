

module ImportAll where

import Data.FunctionExtras

between :: (Fractional a, Ord a) => a -> Bool
between = uncurry (&&) . fork (< 0.5) (> -0.5)


forkWith :: (b -> c -> d) -> (a->b, a->c) -> a -> d
forkWith op = (uncurry op .) . uncurry fork

between' :: (Fractional a, Ord a) => a -> Bool
between' = forkWith (&&) ((< 0.5), (> -0.5))

sprime :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
sprime p q r s = p (q s) (r s)

thingy p q r s t = p (q s) (r t) 
--               = p . fork 