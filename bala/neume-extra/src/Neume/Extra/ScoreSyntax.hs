{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
-- Represent scores with /linear/ sections, repeats, and repeats
-- with alternative endings.
--
--------------------------------------------------------------------------------

module Neume.Extra.ScoreSyntax
  (
  -- * Score ( assembled from /linear/ sections and repeats )
    
    Score(..)
  , ScorePlan(..)

  , Z
  , (:.)
  , TLinear
  , TRepeat
  , TRepAlt

  -- * Operations
  , scoreZipWith
  , content
  
  , stmap

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

-- | A Score with unrepeated (linear) sections, repeats, and 
-- repeats with alternative endings.
--
-- Score using some type level tricks to encode the shape at the
-- type level. This is so that to make polyphonic overlays, for 
-- example, the two scores that are zipped together must have
-- the same shape.
--
data Score shape e where
  Nil     ::                              Score Z e
  Linear  :: e        -> Score shape e -> Score (TLinear :. shape) e
  Repeat  :: e        -> Score shape e -> Score (TRepeat :. shape) e
  RepAlt  :: e -> [e] -> Score shape e -> Score (TRepAlt :. shape) e


-- | A plan of a /Score/ - one datum is carried by each 
-- constructor, rather than many in the case of (Score RepAlt).
-- 
-- This means a ScorePlan can be used for naming the sections in
-- a score, e.g.:
--
-- > type NamedPlan = ScorePlan shape String
--
-- Otherwise all the alternatives in RepAlt would need new names.
-- 
data ScorePlan shape a where
  PNil    ::                           ScorePlan Z a
  PLinear :: a -> ScorePlan shape a -> ScorePlan (TLinear :. shape) a
  PRepeat :: a -> ScorePlan shape a -> ScorePlan (TRepeat :. shape) a
  PRepAlt :: a -> ScorePlan shape a -> ScorePlan (TRepAlt :. shape) a



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


stmap :: (st -> a -> (b,st)) -> st -> Score shape a -> (Score shape b,st)
stmap _ st Nil              = (Nil,st)

stmap f st (Linear e xs)    = (Linear a ys,st'') 
  where (a,st') = f st e; (ys,st'') = stmap f st' xs

stmap f st (Repeat e xs)    = (Repeat a ys,st'')
  where (a,st') = f st e; (ys,st'') = stmap f st' xs

stmap f st (RepAlt e es xs) = (RepAlt a as ys,st''')
  where (a, st')   = f st e
        (as,st'')  = smapList  f st'  es
        (ys,st''') = stmap     f st'' xs

smapList :: (st -> a -> (b,st)) -> st -> [a] -> ([b],st)
smapList _ st []     = ([],st)
smapList f st (x:xs) = (y:ys,st'')
  where (y,st')   = f st x
        (ys,st'') = smapList f st' xs

--------------------------------------------------------------------------------
-- instances

-- Functor

instance Functor (Score shape) where
  fmap _ Nil              = Nil
  fmap f (Linear e xs)    = Linear (f e)            (fmap f xs)
  fmap f (Repeat e xs)    = Repeat (f e)            (fmap f xs)
  fmap f (RepAlt e es xs) = RepAlt (f e) (map f es) (fmap f xs)

instance Functor (ScorePlan shape) where
  fmap _ PNil             = PNil
  fmap f (PLinear e xs)   = PLinear (f e) (fmap f xs)
  fmap f (PRepeat e xs)   = PRepeat (f e) (fmap f xs)
  fmap f (PRepAlt e xs)   = PRepAlt (f e) (fmap f xs)

-- Foldable

instance Foldable (Score shape) where
  foldMap _ Nil              = mempty
  foldMap f (Linear e xs)    = f e `mappend` foldMap f xs
  foldMap f (Repeat e xs)    = f e `mappend` foldMap f xs
  foldMap f (RepAlt e es xs) = f e `mappend` foldMap f es `mappend` foldMap f xs

instance Foldable (ScorePlan shape) where
  foldMap _ PNil             = mempty
  foldMap f (PLinear e xs)   = f e `mappend` foldMap f xs
  foldMap f (PRepeat e xs)   = f e `mappend` foldMap f xs
  foldMap f (PRepAlt e xs)   = f e `mappend` foldMap f xs


-- Traversable

instance Traversable (Score shape) where
  traverse _ Nil              = pure Nil
  traverse f (Linear e xs)    = Linear <$> f e <*> traverse f xs
  traverse f (Repeat e xs)    = Repeat <$> f e <*> traverse f xs
  traverse f (RepAlt e es xs) = RepAlt <$> f e <*> traverse f es <*> traverse f xs


instance Traversable (ScorePlan shape) where
  traverse _ PNil             = pure PNil
  traverse f (PLinear e xs)   = PLinear <$> f e <*> traverse f xs
  traverse f (PRepeat e xs)   = PRepeat <$> f e <*> traverse f xs
  traverse f (PRepAlt e xs)   = PRepAlt <$> f e <*> traverse f xs

-- Show

instance Show e => Show (Score shape e) where
  showsPrec _ Nil               = showString "Nil"

  showsPrec _ (Linear e xs)     = 
    showString "Linear" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (Repeat e xs)     = 
    showString "Repeat" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (RepAlt e es xs)  = 
    showString "RepAlt" . spS . shows e . spS . showList es . spS
                        . appS . spS . shows xs


instance Show e => Show (ScorePlan shape e) where
  showsPrec _ PNil               = showString "PNil"

  showsPrec _ (PLinear e xs)     = 
    showString "PLinear" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (PRepeat e xs)     = 
    showString "PRepeat" . spS . shows e . spS . appS . spS . shows xs

  showsPrec _ (PRepAlt e xs)  = 
    showString "PRepAlt" . spS . shows e . spS . appS . spS . shows xs


spS :: ShowS
spS = showChar ' ' 

appS :: ShowS
appS = showChar '$'


