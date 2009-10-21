{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FunctionExtras
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- More functions on pairs... c.f. Data.Tuple.
-- @fork@ and @prod@ are the useful ones, the others are 
-- somewhat synthetic.
-- 
-----------------------------------------------------------------------------

module Data.FunctionExtras
  ( 
  -- * Combinators
    ( # )
  , subst
  , subst'
  , twine

    
  -- * Pairs
  , fork
  , prod
  , dup
  , swap

  , prodc
  , ufork

  , outer
  , inner
  , firsts
  , seconds

  , undistl
  , distl

  -- * Specs
  , oo
  , ooo
  , oooo

  ) where

--------------------------------------------------------------------------------
-- Combinators

infixl 7 #

-- | Reverse application - the T combinator.
-- Available in Peter Thiemann's Wash and present in 'Client-Side 
-- Web Scripting in Haskell' - Erik Meijer, Daan Leijen & James 
-- Hook.
( # ) :: a -> (a -> b) -> b 
x # f = f x

-- | S combinator - subst.
-- Familiar as Applicative's (<*>) operator:
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

-- Pairs 

--  Functions on pairs - the useful ones @fork@ and @prod@ are 
--  well established [1], some  but unfortunately not present in the 
--  Hierarchical Libraries.
--  .
--  [1] See for instance, Jeremy Gibbons lecture notes 
--  'Calculating Functional Programs'


-- | Apply the functions @f@ and @g@ to the element @a@, 
-- returning the resulting pair. This is not the definition of
-- fork from the literature (see 'ufork'), but is perhaps more 
-- conventional. 
fork :: (a -> b) -> (a -> c) -> a -> (b,c)
fork f g a = (f a,g a)

-- | The /product/ function. 
-- Apply the function @f@ to the first element of the pair,
-- and apply the function @g@ to the second element.
prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

-- exl and exr are fst and snd respectively so are not defined here.

-- | dup aka /Duplicate/.
-- Duplicate the supplied value returning a pair. This is equivalent 
-- to @fork (id,id) a@.
dup :: a -> (a,a)
dup a = (a,a)

-- | Swap the elements of the pair.
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)


-- | A uncurried version of fork - i.e. the functions @f@ and @g@ 
-- are supplied as a pair, Landin-style in the terminology of 
-- Davie. This is the definition of /fork/ given by Gibbons. 
ufork :: (a -> b, a -> c) -> a -> (b,c) 
ufork (f,g) a = (f a, g a)  

-- ufork = uncurry fork


-- | Variant of 'prod' where the supplied is data @(a,b)@ is 
-- extracted from its pair into curried form @... a -> b ...@.
prodc :: (a -> c) -> (b -> d) -> a -> b -> (c,d)
prodc f g = curry $ prod f g


-- | @undistl@.
undistl :: Either (a,b) (a,c) -> (a, Either b c) 
undistl (Left (a,b))  = (a,Left b)
undistl (Right (a,c)) = (a,Right c)

-- | @distl@.
distl :: (a, Either b c) -> Either (a,b) (a,c)
distl (a,Left b)  = Left (a,b)
distl (a,Right c) = Right (a,c)



-- Projections on two pairs

-- | Return the /outer/ elements of the argument pairs.
outer :: (a,b) -> (c,d) -> (a,d)
outer = curry $ prod fst snd

-- | Return the /inner/ elements of the argument pairs.
inner :: (a,b) -> (c,d) -> (b,c)
inner = prodc snd fst

-- | Return the /first/ elements of the argument pairs.
firsts :: (a,b) -> (c,d) -> (a,c)
firsts = prodc fst fst

-- | Return the /second/ elements of the argument pairs.
seconds :: (a,b) -> (c,d) -> (b,d)
seconds = prodc snd snd

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