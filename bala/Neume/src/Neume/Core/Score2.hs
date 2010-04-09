{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Score2
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Minimal syntax types for bars and phrases after rendering to 
-- ABC or Lilypond. 
--
-- Operations - e.g. interspersing with bar lines, adding repeat
-- marks - are simpler and more general when there is almost no 
-- syntax to get in the way. 
--
--------------------------------------------------------------------------------

module Neume.Core.Score2
  (
  -- * Score ( assembled from repeats and /straights/ )
    
    Score(..)

  , Z
  , (:.)
  , TLinear
  , TRepeat
  , TAltRep

  ) where


import Neume.Core.Utils.StateMap

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- Type level list - empty Z, cons (:.)
--
data Z
data x :. xs

data TLinear    
data TRepeat    
data TAltRep

infixr 5 :.

data Score shape e where
  Nil     ::                              Score Z e
  Linear  :: e        -> Score shape e -> Score (TLinear :. shape) e
  Repeat  :: e        -> Score shape e -> Score (TRepeat :. shape) e
  AltRep  :: e -> [e] -> Score shape e -> Score (TAltRep :. shape) e


-- instances

instance Functor (Score shape) where
  fmap _ Nil              = Nil
  fmap f (Linear e xs)    = Linear (f e)            (fmap f xs)
  fmap f (Repeat e xs)    = Repeat (f e)            (fmap f xs)
  fmap f (AltRep e es xs) = AltRep (f e) (map f es) (fmap f xs)

instance Foldable (Score shape) where
  foldMap _ Nil              = mempty
  foldMap f (Linear e xs)    = f e `mappend` foldMap f xs
  foldMap f (Repeat e xs)    = f e `mappend` foldMap f xs
  foldMap f (AltRep e es xs) = f e `mappend` foldMap f es `mappend` foldMap f xs

instance Traversable (Score shape) where
  traverse _ Nil              = pure Nil
  traverse f (Linear e xs)    = Linear <$> f e <*> traverse f xs
  traverse f (Repeat e xs)    = Repeat <$> f e <*> traverse f xs
  traverse f (AltRep e es xs) = AltRep <$> f e <*> traverse f es <*> traverse f xs


instance StateMap (Score shape) where
  stmap _ st Nil              = (Nil,st)
  stmap f st (Linear e xs)    = (Linear e' xs',st'') 
    where  (e', st')    = f st e
           (xs',st'')   = stmap f st' xs

  stmap f st (Repeat e xs)    = (Repeat e' xs', st'')
    where  (e', st')    = f st e
           (xs',st'')   = stmap f st' xs

  stmap f st (AltRep e es xs) = (AltRep e' es' xs', st''')
    where  (e', st')   = f st e
           (es',st'')  = stmap f st' es
           (xs',st''') = stmap f st'' xs




instance Show e => Show (Score shape e) where
  showsPrec _ Nil               = showString "Nil"

  showsPrec _ (Linear e xs)     = 
    showString "Linear" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (Repeat e xs)     = 
    showString "Repeat" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (AltRep e es xs)  = 
    showString "AltRep" . spS . shows e . spS . showList es . spS
                        . appS . spS . shows xs


spS :: ShowS
spS = showChar ' ' 

appS :: ShowS
appS = showChar '$'


