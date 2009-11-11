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
-- Sometimes permuted to be generally useful...
--
-- Note the fixity of @(\#)@ and @(\#\#)@ is not yet /fixed/.
-- Some experience needs to be gathered as to whether the
-- precendence levels are appropriate.
--
-----------------------------------------------------------------------------

module Data.Aviary
  ( 

  -- * The real stuff

    ( # )
  , ( ## )
  , subst
  , bigphi
  , appro
  , pairbimap
  , eitherbimap

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


infixl 8 ##

( ## ) :: (a -> b) -> (b -> c) -> a -> c
f ## g = \x -> g (f x)
 

-- | S combinator - subst.
-- Familiar as Applicative\'s ('<*>') operator, which itself is 
-- fmap:
--
-- f (b -> c) -> f b -> f c where f = ((->) a)
subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x) 

-- | The big Phi, or Turner's @S'@ combinator.
-- Known to Haskell programmers as liftA2 and liftM2 for the 
-- Applicative and Monad instances of (->).
--
-- > (a1 -> a2 -> r) -> m a1 -> m a2 -> m r where m = ((->) a)
-- 
-- Taste suggests you may prefer liftA2.
--
bigphi :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
bigphi f g h x = f (g x) (h x)

-- | A variant of the @D2@ or dovekie combinator - the argument
-- order has been changed to be more satisfying for Haskellers.
--
-- @appro@ is similar to the function @prod@ from the Pair 
-- calculus, but @appro@ applies the first argument 
-- @ f :: (c -> d -> e) @ to the two intermediate results.
-- @prod@ always forms a pair from the intermediate results.
--
-- @on@ from Data.Function is similar but less general, where 
-- the two intermediate results are formed by applying the same 
-- function to the supplied arguments.
--
appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro f g h x y = f (g x) (h y) 



-- | Haskell has no standard Bifunctor class - probably because
-- there aren\'t many natural instances @(,)@, @Either@, trees 
-- with different nodes and leaves...
--
-- Rather than define the Bifunctor class in this dim, distant 
-- corner we just implement the specialization to pairs.  
pairbimap :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
pairbimap f g (x,y) = (f x, g y)


-- | Bimap for 'Either'.
eitherbimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
eitherbimap f _ (Left x)  = Left (f x)
eitherbimap _ g (Right y) = Right (g y)


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