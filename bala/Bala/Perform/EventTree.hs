

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

import Data.Foldable
import Data.Monoid
import Data.Traversable


data EventTree onset evt = EmptyTree 
                         | Next (EventTree onset evt) onset evt 
                         | Par  (EventTree onset evt) evt
                         | Grace (EventTree onset evt) [evt]
                         | Sequence (EventTree onset evt) [EventTree onset evt] 
  deriving (Show)

instance Functor (EventTree onset) where
  fmap f EmptyTree          = EmptyTree
  fmap f (Next t o e)       = Next (fmap f t) o (f e)
  fmap f (Par t e)          = Par (fmap f t) (f e)
  fmap f (Grace t es)       = Grace (fmap f t) (map f es)
  fmap f (Sequence t ts)    = Sequence (fmap f t) (map (fmap f) ts)

instance Foldable (EventTree onset) where
  foldMap f EmptyTree       = mempty
  foldMap f (Next t o e)    = foldMap f t `mappend` f e
  foldMap f (Par t e)       = foldMap f t `mappend` f e
  foldMap f (Grace t es)    = foldMap f t `mappend` foldMap f es
  foldMap f (Sequence t ts) = foldMap f t `mappend` foldMap (foldMap f) ts

  
  
instance Traversable (EventTree onset) where
  traverse f EmptyTree        = pure EmptyTree
  traverse f (Next t o e)     = Next     <$> (traverse f t) <*> pure o <*> (f e)
  traverse f (Par t e)        = Par      <$> (traverse f t) <*> (f e)
  traverse f (Grace t es)     = Grace    <$> (traverse f t) <*> traverse f es
  traverse f (Sequence t ts)  = Sequence <$> (traverse f t) <*> traverse (traverse f) ts

                 
data Note = C | D | E | F | G | A | B
  deriving (Eq,Show)
                           
root = EmptyTree

infixl 7 #

x # f = f x

accumulate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = getConst . traverse (Const . f)

                         
flatten :: EventTree onset a -> [a]
flatten = accumulate (:[])

{- 

flatten :: (Num a) => EventTree a evt -> [(a,evt)]
flatten EmptyTree           = [] 
flatten (Next t onset evt)  = flatten t ++ [(onset,evt)] 
flatten (Par t evt)         = flatten t ++ [(0,evt)]
flatten (Grace t xs)        = flatten t ++ (map (\x-> (0,x)) xs)
flatten (Sequence t xxs)    = flatten t ++ concatMap flatten xxs

-}

note :: onset -> evt -> EventTree onset evt -> EventTree onset evt
note o e t = Next t o e 

chord :: onset -> [evt] -> EventTree onset evt -> EventTree onset evt
chord o (e:es) t = Prelude.foldl (Par) (note o e t) es

grace :: [evt] -> EventTree onset evt -> EventTree onset evt
grace es t = Grace t es

parallel :: [EventTree onset evt] -> EventTree onset evt -> EventTree onset evt
parallel ts t = Sequence t ts

tree1 = root # note 0 C # note 2 C # note 4 D # note 6 E

tree2 = root # chord 0 [C,E,G]

tree3 = root # note 0 C # grace [A,B] # note 2 C # note 4 D # note 6 E

tree4 = root # note 0 C # chord 2 [C,E,G] # note 4 D # note 6 E

tree5 = root # note 0 C # grace [A,B] # chord 2 [C,E,G] # note 4 D # note 6 E


tree6 = root # parallel [treble,bass]
  where treble = tree1
        bass   = root # note 0 C # note 4 D 

tree7 = root # parallel [treble,bass]
  where treble = tree5
        bass   = root # note 0 C # note 4 D 

demo = flatten tree6





