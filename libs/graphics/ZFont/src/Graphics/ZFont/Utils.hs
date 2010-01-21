{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZFont.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils
--
--------------------------------------------------------------------------------


module Graphics.ZFont.Utils where


import Control.Applicative
import qualified Data.Map as Map

  
buildMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map.Map k v
buildMap g h = foldr fn Map.empty where
  fn e m = Map.insert (g e) (h e) m
  



--------------------------------------------------------------------------------



infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2



-- | A variant of the @D2@ or dovekie combinator - the argument
-- order has been changed to be more satisfying for Haskellers:
--
-- > (appro comb f g) x y
--
-- > (f x) `comb` (g y)
-- 
-- @on@ from Data.Function is similar but less general, where 
-- the two intermediate results are formed by applying the same 
-- function to the supplied arguments:
--
-- > on = (appro comb f f)
--
appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro comb f g x y = comb (f x) (g y) 


-- specs - defined in my package data-aviary but defined here to 
-- avoid a dependency

-- | Compose an arity 1 function with an arity 2 function.
-- B1 - blackbird
oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g  
 
  
                   