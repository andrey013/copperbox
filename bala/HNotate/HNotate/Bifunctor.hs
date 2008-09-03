
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Bifunctor
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A class for bimap (c.f fmap) plus left and right specializations.
--
--------------------------------------------------------------------------------

module HNotate.Bifunctor (
    Bifunctor (..), 
    bimapL, 
    bimapR,
    Bicollect(..),
    bicollectL,
    bicollectR,
    Bicrush(..),
    Biproject(..)
  ) where

import Data.Monoid

class Bifunctor s where
  bimap :: (a -> c) -> (b -> d) -> s a b -> s c d
  


bimapL :: Bifunctor s => (a -> c) -> s a b -> s c b
bimapL f = bimap f id

bimapR :: Bifunctor s => (b -> c) -> s a b -> s a c
bimapR f = bimap id f

-- Bicollect collects with a monoid
class Bicollect s where
  bicollect :: Monoid c => (a -> c) -> (b -> c) -> s a b -> c

instance Bicollect Either where
  bicollect pl pr (Left a)  = pl a
  bicollect pl pr (Right b) = pr b
    
instance Bicollect (,) where
  bicollect pl pr (a,b)  = pl a `mappend` pr b
  
  
bicollectL :: (Monoid c, Bicollect s) => (a -> c) -> s a b -> c  
bicollectL f = bicollect f (\_ -> mempty)

bicollectR :: (Monoid c, Bicollect s) => (b -> c) -> s a b -> c  
bicollectR g = bicollect (\_ -> mempty) g

-- bicrush needs two projection functions as well as a fold/accumulator
class Bicrush s where
  bicrushL :: (d -> c -> d) -> (a -> c) -> (b -> c) -> d -> s a b -> d
  bicrushR :: (c -> d -> d) -> (a -> c) -> (b -> c) -> d -> s a b -> d
  
  
instance Bicrush Either where
  bicrushL op pl pr z (Left a)  = z `op` pl a
  bicrushL op pl pr z (Right b) = z `op` pr b 
  bicrushR op pl pr z (Left a)  = pl a `op` z
  bicrushR op pl pr z (Right b) = pr b `op` z
  
instance Bicrush (,) where
  bicrushL op pl pr z (a,b)  = (z `op` pl a) `op` pr b
  bicrushR op pl pr z (a,b)  = pl a `op` (pr b `op` z)


-- Collecting a list rather than a element (or monoid) 
-- as per fl_par & fl_rec in PolyLib.
-- A list nicely handles partialilty (c.f. Nothing in 
-- the Maybe type), and multiple occurances 
-- - e.g pitches in chords or graces notes.
 
class Biproject s where
  biprojectL :: s a b -> [a]
  biprojectR :: s a b -> [b]
  
instance Biproject Either where
  biprojectL (Left a)  = [a]
  biprojectL (Right _) = []
  biprojectR (Left _)  = []
  biprojectR (Right b) = [b]

instance Biproject (,) where
  biprojectL (a,_) = [a]
  biprojectR (_,b) = [b]
        