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
  , dup

  -- * Specs
  , oo
  , ooo
  , oooo

  -- * Combiners
  , combfi
  , combfii
  , combfiii

  ) where


--------------------------------------------------------------------------------
-- Combinators



infixl 7 #

-- | T combinator - thrush
--
-- Reverse application - the T combinator.
-- Found in Peter Thiemann's WASH and the paper 'Client-Side Web 
-- Scripting in Haskell' - Erik Meijer, Daan Leijen & James Hook.
--
( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 8 ##

-- | Q Combinator - the queer bitd.
-- 
-- Reverse composition - found in Peter Thiemann's WASH.
-- You might perfer to use (<<<) from Control.Categoty.
--
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
-- Taste suggests you may prefer liftA2 especially as @bigphi@ is
-- not a great name (calling it s\' would take a very useful 
-- variable name).
--
bigphi :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
bigphi f g h x = f (g x) (h x)

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


-- | dup - duplicator aka the W combinator aka Warbler. 
-- 
-- > dup f x = f x x
--
dup :: (a -> a -> b) -> a -> b
dup f x = f x x





--------------------------------------------------------------------------------
-- Specs - blackbird, bunting, ...

-- Alleviate your composing-sectioning mania with specs!
--
-- E.g.:
-- (abs .) . (*) ==> abs `oo` (*)
--
-- The family name /specs/ (glasses, specs, lunettes) is a 
-- visual pun when infix directives @`oo`@ are included. The 
-- @o@\'s of individual combinators are a fraternal nod to 
-- Clean and ML who use @o@ as function composition. Naturally
-- we don\'t defined @o@ here and waste a good variable on a 
-- redundant combinator.

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
-- Combiners


-- | Combiners - similar to the cardinal\' combinator. 
--
-- Mnemonically - @comb@(ine) after applying @f@ to @x@ and a 
-- single identity: @y@. 
--
-- > combfi comb f x y = comb (f x) y
--
-- Equivalently:
--
-- > combfi comb f = appro comb f id
-- 
-- But combfi is a useful introduction to the (somewhat manic, 
-- but sometimes useful) higher arity versions.
-- 
combfi :: (c -> b -> d) -> (a -> c) -> a -> b -> d
combfi comb f x y = comb (f x) y 
 
-- | Extrapolation of 'combfi' with another identity.
--
-- Mnemonically - comb(ine) after applying @f@ to @x@ and two 
-- identities: @y@ and @z@.
--
-- > combfii comb f x y z = comb (f x) y z
--
combfii :: (d -> b -> c -> e) -> (a -> d) -> a -> b -> c -> e
combfii comb f x y z = comb (f x) y z  

-- | Extrapolation of 'combfii' with a further identity.
--
-- Mnemonically - comb(ine) after applying @f@ to @s@ and three
-- identities: @t@ and @u@ and @v@.
--
-- > combfii comb f s t u v = comb (f s) t u v
--
combfiii :: (e -> b -> c -> d -> f) -> (a -> e) -> a -> b -> c -> d -> f
combfiii comb f s t u v = comb (f s) t u v  
