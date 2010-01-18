{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.Functional
-- Copyright   :  (c) Stephen Peter Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Functor, Applicative, Monad functions /specialized/ to 
-- functions.
--
-- This is for reference (obviously!) and is not intended for use.
--
-----------------------------------------------------------------------------

module Data.Aviary.Functional
  ( 
  -- * Functor
    fmap

  -- * Applicative
  , (<$>)
  , (<$)

  , pure
  , (<*>)
  , (*>)
  , (<*)

  , (<**>)
  , liftA, liftA2, liftA3
  
  -- * Category
  , id
  , (.)
  , (<<<)
  , (>>>)

  -- * Monad
  , (>>=)
  , (>>) 
  , return
  , fail  
  , mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  ) where

import qualified Control.Applicative  as Ap
import qualified Control.Category     as Cat
import qualified Control.Monad        as Mon


import Prelude ( String )

--------------------------------------------------------------------------------
-- Functor

-- ((->) r) replaces the type variable f

-- | The fmap instance for functions is in 
-- Control.Monad.Instances.

fmap        :: (a -> b) -> (r -> a) -> (r -> b)
fmap        = Mon.fmap


--------------------------------------------------------------------------------
-- Control.Applicative

-- ((->) r) replaces the type variable f

(<$>)       :: (a -> b) -> (r -> a) -> (r -> b)
(<$>)       = Mon.fmap

(<$)        :: a -> (r -> b) -> (r -> a)
(<$)        = (Ap.<$)

-- Applicative class

pure        :: a -> (r -> a)
pure        = Ap.pure

(<*>)       :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*>)       = (Ap.<*>)

(*>)        :: (r -> a) -> (r -> b) -> (r -> b)
(*>)        = (Ap.*>)

(<*)        :: (r -> a) -> (r -> b) -> (r -> a)
(<*)        = (Ap.<*)

-- No function instance of Alternative.



(<**>)      :: (r -> a) -> (r -> a -> b) -> (r -> b)
(<**>)      = (Ap.<**>)

liftA       :: (a -> b) -> (r -> a) -> (r -> b)
liftA       = Ap.liftA

liftA2      :: (a -> b -> c) -> (r -> a) -> (r -> b) -> (r -> c)
liftA2      = Ap.liftA2

liftA3      :: (a -> b -> c -> d) -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d)
liftA3      = Ap.liftA3

-- No optional (due to no Alternative instance)



--------------------------------------------------------------------------------
-- Control.Category

-- (->) replaces the type variable cat

id          :: a -> a
id          = Cat.id

(.)         ::  (b -> c) -> (a -> b) -> (a -> c)
(.)         = (Cat..)

(<<<)       :: (b -> c) -> (a -> b) -> (a -> c)
(<<<)       = (Cat.<<<)

(>>>)       :: (a -> b) -> (b -> c) -> (a -> c)
(>>>)       = (Cat.>>>)

--------------------------------------------------------------------------------
-- Control.Monad

-- ((->) r) replaces the type variable m

(>>=)       :: (r -> a) -> (a -> r -> b) -> (r -> b)
(>>=)       = (Mon.>>=)

(>>)        :: (r -> a) -> (r -> b) -> (r -> b)
(>>)        = (Mon.>>)

return      :: a -> (r -> a)
return      = Mon.return

fail        :: String -> (r -> a)
fail        = Mon.fail

-- No function instance of MonadPlus

mapM        :: (a -> r -> b) -> [a] -> r -> [b]
mapM        = Mon.mapM

mapM_       :: (a -> r -> b) -> [a] -> r -> ()
mapM_       = Mon.mapM_

forM        :: [a] -> (a -> r -> b) -> r -> [b]
forM        = Mon.forM

forM_       :: [a] -> (a -> r -> b) -> r -> ()
forM_       = Mon.forM_


sequence    :: [r -> a] -> r -> [a]
sequence    = Mon.sequence
 
sequence_   :: [r -> a] -> r -> ()
sequence_   = Mon.sequence_

{-
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
forever :: Monad m => m a -> m b
join :: Monad m => m (m a) -> m a
-}

