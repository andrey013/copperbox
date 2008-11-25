
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.FocusedShapeContents
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


module Bala.Base.FocusedShapeContents ( 
    FShape, -- don't export the constructors
    Focus(..),
    separate, rejoin
  ) where

import Bala.Base.BaseExtra (fork)

import Control.Monad.State

import qualified Data.Foldable as F
import Data.Maybe
import Data.Sequence hiding (reverse)
import qualified Data.Traversable as T

data FShape a = Focus a | Obliv a
  
  
instance Show (FShape a) where
  show (Focus _)  = "(*)" 
  show (Obliv _)  = "(_)"   

              

data Focus a b = FN { focus   :: a -> Bool, 
                      extract :: a -> b, 
                      putback :: a -> b -> a } 


separate :: (Functor t, F.Foldable t) => Focus a b -> t a -> (t (FShape a), [b])
separate fcs = fork (fmap f) (F.foldr g []) where
    f a    = if (focus fcs) a then Focus a else Obliv a
    g a xs = if (focus fcs) a then (extract fcs) a : xs else xs

    
rejoin :: T.Traversable t => Focus a b -> (t (FShape a), [b]) -> t a
rejoin fcs (t, xs) = evalState (T.mapM getElement t) xs where
    -- getElement :: (FShape a) -> State [b] a
    getElement (Obliv a)  = return a
    getElement (Focus a)  = do xs <- get
                               case xs of
                                 x:xs -> do put xs
                                            return $ (putback fcs) a x
                                 []   -> fail "rejoin - contents too short"
                                 
                                                             