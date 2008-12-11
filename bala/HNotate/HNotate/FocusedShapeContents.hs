{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.FocusedShapeContents
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A specialization of shape and contents traversals with user defined focus 
--
--------------------------------------------------------------------------------


module HNotate.FocusedShapeContents ( 
    FShape, -- don't export the constructors
    Focus(..),
    separate, rejoin,
    shapeContentsTraversal
  ) where


import Control.Monad.State

import qualified Data.Foldable as F
import qualified Data.Traversable as T

data FShape a = Focus a | Obliv a
  
  
instance Show (FShape a) where
  show (Focus _)  = "(*)" 
  show (Obliv _)  = "(_)"   

-- Focus holds 3 functions@
-- @focus@ is a predicate to select elements of interest.
-- @extract@ gets a value form an element (it can be just part of an element
-- hence the different type.
-- @putback@ - updates an element with an extracted (and modified) value
data Focus a b = FN { focus   :: a -> Bool, 
                      extract :: a -> b, 
                      putback :: b -> a -> a } 


separate :: (Functor t, F.Foldable t) => Focus a b -> t a -> (t (FShape a), [b])
separate fcs = fork (fmap f) (F.foldr g []) where
    f a    = if (focus fcs) a then Focus a else Obliv a
    g a xs = if (focus fcs) a then (extract fcs) a : xs else xs
    
fork :: (a -> b) -> (a -> c) -> a -> (b,c) 
fork f g a = (f a, g a)  
    
rejoin :: T.Traversable t => Focus a b -> (t (FShape a), [b]) -> t a
rejoin fcs (t, ss) = evalState (T.mapM getElement t) ss where
    -- getElement :: (FShape a) -> State [b] a
    getElement (Obliv a)  = return a
    getElement (Focus a)  = do xs <- get
                               case xs of
                                 y:ys -> do put ys
                                            return $ (putback fcs) y a
                                 []   -> fail "rejoin - contents too short"
                                 

shapeContentsTraversal :: 
          T.Traversable t => Focus a b -> ([b] -> [b]) -> t a -> t a
shapeContentsTraversal fcs fn = rejoin fcs . change . separate fcs
  where
    change (shape,contents) = (shape, fn contents)
                               