
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.EventTree
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A complicated tree 
--
--------------------------------------------------------------------------------

module Bala.Perform.EventTree where


import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable




import Prelude hiding (sequence)


-- Tracks in MIDI and multiple staffs are represented as a list of
-- Event trees
newtype Performance evt = Perf { unPerf :: [EventTree evt] }
  deriving Show
  
data EventTree evt = EmptyTree
                   | Next (EventTree evt) evt 
                   | Par  (EventTree evt) evt
                   | Prefix evt (EventTree evt)
                   | Sequence (EventTree evt) [EventTree evt]                   
  deriving Show

root            :: EventTree evt
root            = EmptyTree

note            :: evt -> EventTree evt -> EventTree evt
note e t        = Next t e 

chord           :: [evt] -> EventTree evt -> EventTree evt
chord (e:es) t  = let t' = Prelude.foldl (Par) t es in Next t' e


-- maybe grace should take an event 
-- and do its subtractions before it is added to the tree
grace           :: [evt] -> EventTree evt -> EventTree evt
grace es t      = Prelude.foldl (flip Prefix) t es

parallel        :: [EventTree evt] -> EventTree evt -> EventTree evt
parallel ts t   = Sequence t ts


infixl 7 #

x # f = f x

instance Functor EventTree where
  fmap f EmptyTree          = EmptyTree
  fmap f (Next t e)         = Next (fmap f t) (f e) 
  fmap f (Par t e)          = Par (fmap f t) (f e)
  fmap f (Prefix e t)       = Prefix (f e) (fmap f t) 
  fmap f (Sequence t ts)    = Sequence (fmap f t) (map (fmap f) ts)

instance Foldable EventTree where
  foldMap f EmptyTree       = mempty
  foldMap f (Next t e)      = foldMap f t `mappend` f e
  foldMap f (Par t e)       = foldMap f t `mappend` f e
  foldMap f (Prefix e t)    = f e `mappend` foldMap f t
  foldMap f (Sequence t ts) = foldMap f t `mappend` foldMap (foldMap f) ts
  
  
instance Traversable EventTree where
  traverse f EmptyTree        = pure EmptyTree
  traverse f (Next t e)       = Next     <$> (traverse f t) <*> (f e)
  traverse f (Par t e)        = Par      <$> (traverse f t) <*> (f e)
  traverse f (Prefix e t)     = Prefix   <$> (f e) <*> (traverse f t)
  traverse f (Sequence t ts)  = Sequence <$> (traverse f t) 
                                         <*> traverse (traverse f) ts

instance Functor Performance where
  fmap f (Perf xs)            = Perf (map (fmap f) xs) 

instance Foldable Performance where
  foldMap f (Perf xs)         = foldMap (foldMap f) xs
  
instance Traversable Performance where
  traverse f (Perf xs)        = Perf <$> traverse (traverse f) xs
  
accumulate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = getConst . traverse (Const . f)

                         
flatten :: EventTree a -> [a]
flatten = accumulate (:[])

  