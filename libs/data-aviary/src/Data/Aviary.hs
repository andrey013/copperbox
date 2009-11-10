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
-- Bird combinators
-- 
-----------------------------------------------------------------------------

module Data.Aviary
  ( 
  -- * Data.Function combinators as birds
    idiot
  , kestrel
  , bluebird
  , cardinal
  , applicator
  , psi

  -- * The real stuff

  ,  ( # )
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

-- Bird named versions from Data.Function




-- | I combinator - identity bird / idiot - Haskell 'id'.
idiot :: a -> a 
idiot = id

-- | K combinator - kestrel - Haskell 'const'.
kestrel :: a -> b -> a
kestrel = const

-- | B combinator - bluebird - Haskell ('.').
bluebird :: (b -> c) -> (a -> b) -> a -> c
bluebird = (.)


-- | C combinator - cardinal - Haskell 'flip'.
cardinal :: (a -> b -> c) -> b -> a -> c
cardinal = flip

-- | A combinator - apply / applicator - Haskell ('$').
applicator :: (a -> b) -> a -> b
applicator = ($)

-- 'fix' unknown

-- | Psi combinator - psi bird (?) - Haskell 'on'.  
psi :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi = on



--------------------------------------------------------------------------------


infixl 7 #

-- | T combinator - thrush
--
-- Reverse application - the T combinator.
-- Available in Peter Thiemann's Wash and present in 'Client-Side 
-- Web Scripting in Haskell' - Erik Meijer, Daan Leijen & James 
-- Hook.
( # ) :: a -> (a -> b) -> b 
x # f = f x

-- | S combinator - subst.
-- Familiar as Applicative\'s ('<*>') operator:
-- f (b -> c) -> f b -> f c where f = ((->) a)
subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g a = f a (g a) 

-- | The big Phi, or Turner's @S'@ combinator.
-- Known to Haskell programmers as liftM2 when written:
-- (a1 -> a2 -> r) -> m a1 -> m a2 -> m r where m = ((->) a)
subst' :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
subst' p q r s = p (q s) (r s)


-- | A variant of the @D2@ or dovekie combinator - the argument
-- order has been changed, to be more for Haskellers.
twine :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
twine p q r s t = p (q s) (r t) 


--------------------------------------------------------------------------------
-- Specs

-- Alleviate your composing-sectioning mania with specs!
-- The name becomes a pun on spectacles (glasses, specs), 
-- once you use infix directives @`oo`@.
-- E.g.:
-- (abs .) . (*) ==> abs `oo` (*)

-- | Compose an arity 1 function with an arity 2 function.
oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

-- | Compose an arity 1 function with an arity 3 function.
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g  