{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.Birds
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Bird monickered combinators
-- 
-----------------------------------------------------------------------------

module Data.Aviary.Birds
  ( 
  -- * Data.Function combinators as birds
    idiot
  , kestrel
  , bluebird
  , cardinal
  , applicator
  , psi
  
  -- * Other birds
  , warbler
  , starling
  , jay
  , robin
  , vireo
  , finch
  , dove
  , blackbird
  , bunting
  , becard
  , eagle
  , dickcissel
  , dovekie
  , queer
  , quixotic
  , quizzical
  , quirky
  , quacky
  , goldfinch
  , cardinal'
  , hummingbird

  ) where

import Data.Function

--------------------------------------------------------------------------------
-- Combinators

-- Bird named versions from Data.Function




-- | I combinator - identity bird / idiot bird - Haskell 'id'.
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
-- Other birds

-- | W combinator - warbler - elementary duplicator.
warbler :: (a -> a -> b) -> a -> b
warbler f x = f x x



-- | S combinator - starling - Haskell: Applicative\'s (<*>) on 
-- functions.
--
-- Substitution.
starling :: (a -> b -> c) -> (a -> b) -> a -> c
starling f g x = f x (g x)


-- | J combinator - jay.
jay :: (a -> b -> b) -> a -> b -> a -> b
jay f x y z = f x (f z y)


-- | R combinator - robin.
robin :: a -> (b -> a -> c) -> b -> c
robin x f y = f y x 

-- | V combinator - vireo.
vireo :: a -> b -> (a -> b -> c) -> c
vireo x y f = f x y

-- | F combinator - finch.
finch :: a -> b -> (b -> a -> c) -> c
finch x y f = f y x

-- | D combinator - dove.
dove :: (a -> c -> d) -> a -> (b -> c) -> b -> d
dove f x g y = f x (g y)

-- | B1 combinator - blackbird - specs `oo`.
blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird f g x y = f (g x y)

-- | B2 combinator - bunting - specs `ooo`.
bunting :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
bunting f g x y z = f (g x y z)

-- | B3 combinator - becard.
becard :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
becard f g h x = f (g (h x))


-- | E combinator - eagle.
eagle :: (a -> d -> e) -> a -> (b -> c -> d) -> b -> c -> e
eagle f x g y z = f x (g y z) 


-- | D1 combinator - dickcissel.
dickcissel :: (a -> b -> d -> e) -> a -> b -> (c -> d) -> c -> e
dickcissel f x y g z = f x y (g z)

-- | D2 combinator - dovekie.
dovekie :: (c -> d -> e) -> (a -> c) -> a -> (b -> d) -> b -> e
dovekie f g x h z = f (g x) (h z)


-- | Q combinator - queer bird.
-- Haskell (##) in Peter Thiemann\'s Wash, reverse composition.
queer :: (a -> b) -> (b -> c) -> a -> c
queer f g x = g (f x)

-- | Q1 combinator - quixotic bird.
quixotic :: (b -> c) -> a -> (a -> b) -> c
quixotic f x g = f (g x)

-- | Q2 combinator - quizzical bird.
quizzical :: a -> (b -> c) -> (a -> b) -> c
quizzical x f g = f (g x)

-- | Q3 combinator - quirky bird.
quirky :: a -> b -> (a -> b -> c) -> c
quirky x y f = f x y

-- | Q4 combinator - quacky bird.
quacky :: a -> (a -> b) -> (b -> c) -> c 
quacky x f g = g (f x)


-- | G combinator - goldfinch.
goldfinch :: (b -> c -> d) -> (a -> c) -> a -> b -> d
goldfinch f g x y = f y (g x)

-- | C' combinator - no name.
cardinal' :: (c -> a -> d) -> (b -> c) -> a -> b -> d
cardinal' f g x y = f (g y) x


-- | H Combinator - hummingbird.
hummingbird :: (b -> a -> b -> c) -> a -> b -> c 
hummingbird f x y = f y x y