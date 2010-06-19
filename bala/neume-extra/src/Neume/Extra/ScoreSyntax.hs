{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.ScoreSyntax
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

module Neume.Extra.ScoreSyntax
  (
  -- * Score ( assembled from repeats and /straights/ )
    
    Score(..)

  , Z
  , (:.)
  , TLinear
  , TRepeat
  , TRepAlt

  , scoreZipWith
  , content

  , scoreFoldl'
  , scoreFoldr

  ) where



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
data TRepAlt

infixr 5 :.

data Score shape e where
  Nil     ::                              Score Z e
  Linear  :: e        -> Score shape e -> Score (TLinear :. shape) e
  Repeat  :: e        -> Score shape e -> Score (TRepeat :. shape) e
  RepAlt  :: e -> [e] -> Score shape e -> Score (TRepAlt :. shape) e


scoreZipWith :: (a -> b -> c) -> Score sh a -> Score sh b -> Score sh c
scoreZipWith _ Nil              Nil                = Nil

scoreZipWith f (Linear a xs)    (Linear b ys)    = 
    Linear (f a b) $ scoreZipWith f xs ys

scoreZipWith f (Repeat a xs)    (Repeat b ys)    = 
    Repeat (f a b) $ scoreZipWith f xs ys

scoreZipWith f (RepAlt a as xs) (RepAlt b bs ys) = 
    RepAlt (f a b) (zipWith f as bs) $ scoreZipWith f xs ys

scoreZipWith _ _                _                = 
    error $ "scoreZipWith - unreachable in theory..."

content :: Score shape e -> [e]
content Nil              = []
content (Linear e xs)    = e : content xs
content (Repeat e xs)    = e : content xs
content (RepAlt e es xs) = (e:es) ++ content xs

scoreFoldl' :: e -> (e -> a -> e) -> (e -> a -> e) -> (e -> a -> [a] -> e) 
            -> Score shape a -> e
scoreFoldl' nil lin rep repa s0 = foldl_go nil lin rep repa s0

foldl_go :: e -> (e -> a -> e) -> (e -> a -> e) -> (e -> a -> [a] -> e) 
         -> Score shape a -> e
foldl_go ac _   _   _    Nil              = ac

foldl_go ac lin rep repa (Linear e xs)    = 
    let z' = lin ac e in z' `seq` (foldl_go z' lin rep repa xs)  

foldl_go ac lin rep repa (Repeat e xs)    = 
    let z' = rep ac e in z' `seq` (foldl_go z' lin rep repa xs)

foldl_go ac lin rep repa (RepAlt e es xs) = 
    let z' = repa ac e es in z' `seq` (foldl_go z' lin rep repa xs)  


scoreFoldr :: e -> (a -> e -> e) 
           -> (a -> e -> e) -> (a -> [a] -> e -> e) 
           -> Score shape a -> e
scoreFoldr nil _   _   _    Nil              = nil

scoreFoldr nil lin rep repa (Linear e xs)    = 
    lin e (scoreFoldr nil lin rep repa xs)

scoreFoldr nil lin rep repa (Repeat e xs)    = 
    rep e (scoreFoldr nil lin rep repa xs)

scoreFoldr nil lin rep repa (RepAlt e es xs) = 
    repa e es (scoreFoldr nil lin rep repa xs)


--------------------------------------------------------------------------------
-- instances

instance Functor (Score shape) where
  fmap _ Nil              = Nil
  fmap f (Linear e xs)    = Linear (f e)            (fmap f xs)
  fmap f (Repeat e xs)    = Repeat (f e)            (fmap f xs)
  fmap f (RepAlt e es xs) = RepAlt (f e) (map f es) (fmap f xs)

instance Foldable (Score shape) where
  foldMap _ Nil              = mempty
  foldMap f (Linear e xs)    = f e `mappend` foldMap f xs
  foldMap f (Repeat e xs)    = f e `mappend` foldMap f xs
  foldMap f (RepAlt e es xs) = f e `mappend` foldMap f es `mappend` foldMap f xs

instance Traversable (Score shape) where
  traverse _ Nil              = pure Nil
  traverse f (Linear e xs)    = Linear <$> f e <*> traverse f xs
  traverse f (Repeat e xs)    = Repeat <$> f e <*> traverse f xs
  traverse f (RepAlt e es xs) = RepAlt <$> f e <*> traverse f es <*> traverse f xs



instance Show e => Show (Score shape e) where
  showsPrec _ Nil               = showString "Nil"

  showsPrec _ (Linear e xs)     = 
    showString "Linear" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (Repeat e xs)     = 
    showString "Repeat" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (RepAlt e es xs)  = 
    showString "RepAlt" . spS . shows e . spS . showList es . spS
                        . appS . spS . shows xs


spS :: ShowS
spS = showChar ' ' 

appS :: ShowS
appS = showChar '$'


