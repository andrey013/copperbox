{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Plainly named combinators
-- 
-----------------------------------------------------------------------------

module Data.Aviary
  ( 

  -- * The real stuff

    ( # )
  , subst
  , subst'
  , twine

    

  -- * Specs
  , oo
  , ooo
  , oooo

  ) where

import Data.Function

--------------------------------------------------------------------------------
-- Combinators



infixl 7 #

-- | T combinator - thrush
--
-- Reverse application - the T combinator.
-- Found in Peter Thiemann's Wash and the paper 'Client-Side Web 
-- Scripting in Haskell' - Erik Meijer, Daan Leijen & James Hook.
( # ) :: a -> (a -> b) -> b 
x # f = f x

-- | S combinator - subst.
-- Familiar as Applicative\'s ('<*>') operator:
-- f (b -> c) -> f b -> f c where f = ((->) a)
subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x) 

-- | The big Phi, or Turner's @S'@ combinator.
-- Known to Haskell programmers as liftM2 when written:
-- (a1 -> a2 -> r) -> m a1 -> m a2 -> m r where m = ((->) a)
subst' :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
subst' f g h x = f (g x) (h x)


-- | A variant of the @D2@ or dovekie combinator - the argument
-- order has been changed, to be more for Haskellers.
twine :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
twine f g h x y = f (g x) (h y) 


--------------------------------------------------------------------------------
-- Specs - blackbird, bunting, ...

-- Alleviate your composing-sectioning mania with specs!
-- The name becomes a pun on spectacles (glasses, specs), 
-- once you use infix directives @`oo`@.
-- E.g.:
-- (abs .) . (*) ==> abs `oo` (*)

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