{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Arity.Functions.Specs
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Arity extended combinators
-- 
-- Composition
--
--------------------------------------------------------------------------------

module Data.Arity.Functions.Specs
  (
    oo
  , ooo
  , oooo
  , ooooo

  ) where

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
oo f g s t = f (g s t)

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g s t u = f (g s t u)

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g s t u v = f (g s t u v)


-- | Compose an arity 1 function with an arity 5 function.
ooooo :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
ooooo f g s t u v w = f (g s t u v w) 
