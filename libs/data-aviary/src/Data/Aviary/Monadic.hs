{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.Monadic
-- Copyright   :  (c) Stephen Peter Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- An collection of monadic variants.
--
-- Strong caution - the naming in this module is provisional. 
-- A systematic approach is needed... (but which sysytem?)
--
-----------------------------------------------------------------------------

module Data.Aviary.Monadic
  ( 
   
  -- $identity

    constM
  , stripM

  -- $bluebird

  , cardinalM
  , mcardinal

  , ifte
  , condM 
  , mcondM

  , condE
  , mcondE

  , using
  , usingM
  , usingE

  ) where

import Control.Monad ( liftM )

-- $identity
--
-- Starting from id...
--
-- The monadic analogy to @id :: a -> a@ is @return a -> m a@.
--


-- const and strip are not very exciting...
-- 
-- Even monadically, plain const and split are likely to have 
-- more uses.
--

-- | 'constM' promotes the first of its arguments to the monad.
--
constM :: Monad m => a -> b -> m a
constM a _ = return a


-- | 'stripM' promotes the second of its arguments to the monad.
--
stripM :: Monad m => a -> b -> m b
stripM _ b = return b


-- $bluebird
--
-- Monadic composition (cf. (.) aka bluebird) is the Kleisli operator
-- @(<=<) @.
--
-- > composeM :: Monad m => (b -> m c) -> (a -> m b) a -> m c
--
--
-- Monadic reverse composition (cf. (#) aka queer) is the Kleisli 
-- operator @(>=>) @.
--
-- > revcomposeM :: Monad m => (a -> m b) -> (b -> m c) a -> m c
--



-- | 'cardinalM' -- monadic flip.
-- 
-- Superficially there are 2 reasonable variations.
-- 
-- First, monadic combiner, monadic args
--
-- > m1 :: Monad m => (a -> b -> m c) -> b -> a -> m c
--
-- Second, pure combiner, monadic args
--
-- > m2 :: Monad m => (a -> b -> c) -> m b -> m a -> m c
--
-- Monadic combiner, pure args is @flip@ itself.
--

cardinalM :: Monad m => (a -> b -> m c) -> m b -> m a -> m c
cardinalM mf mb ma = mb >>= \b -> ma >>= \a -> mf a b

mcardinal :: Monad m => (a -> b -> c) -> m b -> m a -> m c
mcardinal f mb ma = mb >>= \b -> ma >>= \a -> return (f a b)

--  'applicatorM' -- monadic apply.





-- Note to me - the variant where the test is a plain value
-- (not monadic) is captured by the \'normal\' if_then_else 
--
-- > ifte :: Bool -> m a -> m a -> m a
--
-- ... is straight-forwardly
--
-- > ifte :: Bool -> a -> a -> a 
--

ifte :: Monad m => m Bool -> m a -> m a -> m a
ifte mtest sk fk = mtest >>= \ans -> if ans then sk else fk


-- condM has two variations one with a plain Bool test and the
-- other with a monadic Bool.

condM :: Monad m => Bool -> m a -> m (Maybe a)
condM test sk = if test then liftM Just sk else return Nothing

mcondM :: Monad m => m Bool -> m a -> m (Maybe a)
mcondM mtest sk = mtest >>= \ans ->
                  if ans then liftM Just sk else return Nothing 


condE :: Monad m => Bool -> m sa -> m fa -> m (Either fa sa)
condE test sk fk = if test then liftM Right sk else liftM Left fk


mcondE :: Monad m => m Bool -> m sa -> m fa -> m (Either fa sa)
mcondE mtest sk fk = mtest >>= \ans -> 
                     if ans then liftM Right sk  else liftM Left fk


-- Use a value in the test and success and failure continuations...
--
-- Any value in the following variant?
--
-- > f1 :: Monad m => (a -> Bool) -> (a -> m b) -> (a -> m b) -> a -> m b
--


using :: Monad m => (a -> m Bool) -> (a -> m b) -> (a -> m b) -> a -> m b
using mtest sk fk a = mtest a >>= \ans -> 
                      if ans then (sk a) else (fk a)

usingM :: Monad m => (a -> m Bool) -> (a -> m b) -> a -> m (Maybe b)
usingM mtest sk a = mtest a >>= \ans -> 
                    if ans then liftM Just (sk a) else return Nothing

usingE :: Monad m 
       => (a -> m Bool) -> (a -> m sa) -> (a -> m fa) -> a -> m (Either fa sa)
usingE mtest sk fk a = mtest a >>= \ans -> 
                       if ans then liftM Right (sk a) else liftM Left (fk a)

