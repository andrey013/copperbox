{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Utils
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Combinators, utils....
--
--------------------------------------------------------------------------------


module Wumpus.Extra.Utils
  (

  -- * Para
    para

  -- * Specs
  , oo
  , ooo
  , oooo

  -- * pstars
  , pstar
  , pstar2
  , pstar3
  , pstar4
  , pstar5

  -- * Combinators
  , appro
  , combfi
  , dup

  -- * Hughes list
  , H 
  , toListH
  , consH

  ) where



--------------------------------------------------------------------------------

-- | paramorphism (generalizes cata (foldr), folds right...)
para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi b = step
  where step []     = b
        step (x:xs) = phi x (xs, step xs)



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


--------------------------------------------------------------------------------
-- pstars

pstar     :: (a -> r -> ans) 
          -> (r -> a) 
          -> r -> ans
pstar f fa x = f (fa x) x

pstar2    :: (a -> b -> r -> ans) 
          -> (r -> a) -> (r -> b) 
          -> r -> ans
pstar2 f fa fb x = f (fa x) (fb x) x

pstar3    :: (a -> b -> c -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) 
          -> r -> ans
pstar3 f fa fb fc x = f (fa x) (fb x) (fc x) x

pstar4    :: (a -> b -> c -> d -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) 
          -> r -> ans
pstar4 f fa fb fc fd x = f (fa x) (fb x) (fc x) (fd x) x

pstar5    :: (a -> b -> c -> d -> e -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> (r -> e) 
          -> r -> ans
pstar5 f fa fb fc fd fe x = f (fa x) (fb x) (fc x) (fd x) (fe x) x


-- NEEDS NEW NAME!
-- > on = (appro comb f f)
--
appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro bop f g x y = (f x) `bop` (g y) 


-- 
combfi :: (c -> b -> d) -> (a -> c) -> a -> b -> d
combfi bop f x y = (f x) `bop` y 

-- | dup - duplicator aka the W combinator aka Warbler. 
-- 
-- > dup f x = f x x
--
dup :: (a -> a -> b) -> a -> b
dup f x = f x x


--------------------------------------------------------------------------------

type H a = [a] -> [a]

toListH :: H a -> [a]
toListH = ($ [])

consH :: a -> H a -> H a
consH a hl = (a:) . hl