{-# OPTIONS -Wall #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FPList
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- A fence-post-list datatype. It is polymorphic on two types and 
-- statically ensures that the outer type @a@ bookends and intersperses 
-- the inner type @b@ correctly. For example, it generates
-- a, aba, ababa, abababa, etc.  
--
--------------------------------------------------------------------------------

module Data.FPList 
  ( 
    FPList

  , length
  , singleton
  , cons

  , inners
  , outers

  ) where

import Prelude hiding (length)

-- The main data type is a 'fence post list' - a list with two 
-- elements types. There is a static guarantee that there is one 
-- more outer element (fence-post) than inner element (fence-section),
-- and types are correctly interspersed: eg. a, aba, ababa, etc. 
-- Note, that there is no way to construct an empty FPList, the 
-- smallest list must be a singleton.
data FPList a b = FPList a (FPList' a b)
  deriving (Eq)
  
data FPList' a b = End 
                 | Next b (FPList a b)
  deriving (Eq)


instance (Show a, Show b) => Show (FPList a b) where
  showsPrec i (FPList e rs) = showString "<|" . showsPrec i e . showl' rs
    where
     showl (FPList a xs) = showsPrec i a . showl' xs
     showl' End         = showString "|>"
     showl' (Next b xs) = showChar ',' . showsPrec i b . showChar ',' . showl xs
       

singleton :: a -> FPList a b
singleton a = FPList a End

-- | Consing to a /fence-post-list/ needs both an @a@ and a @b@. 
cons :: a -> b -> FPList a b -> FPList a b
cons a b rest = FPList a (Next b rest)

-- | Length of an FPList.  
length :: FPList a b -> Int
length (FPList _ End)           = 1
length (FPList _ (Next _ rest)) = 2 + length rest 


-- | Extract the inner elements of the fence-post list.
inners :: FPList a b -> [b]
inners (FPList _ End)         = []
inners (FPList _ (Next b xs)) = b : inners xs

-- | Extract the inner elements of the fence-post list.
outers :: FPList a b -> [a]
outers (FPList a End)         = [a]
outers (FPList a (Next _ xs)) = a : outers xs



{-



-- Build a fence-post-list with a prefix of @a@ and a list of @(b,a)@ pairs 
zipbuild :: a -> [(b,a)] -> FPList a b
zipbuild a []           = singleton a
zipbuild a ((b,a'):xs)  = cons a b rest where rest = zipbuild a' xs


merge :: (a -> c) -> (b -> c) -> FPList a b -> [c]
merge f _ (FPList a End)            = [f a]
merge f g (FPList a (Next b rest))  = f a : g b : merge f g rest


-- @bimerge@ is similar to the apomorphism recursion scheme.
-- It merges pair-wise up to the last @a@ element which it has 
-- to /flush/. 
bimerge :: (a -> b -> c) -> (a -> c) -> FPList a b -> [c]
bimerge _ apo (FPList a End)            = [apo a]
bimerge f apo (FPList a (Next b rest))  = f a b : bimerge f apo rest


data FPView a b = Sgl a
                | DCons a b (FPList a b)
  deriving (Show)              

fpview :: FPList a b -> FPView a b  
fpview (FPList a End)           = Sgl a
fpview (FPList a (Next b rest)) = DCons a b rest

-- @knitWith@ truncates the normal lists like zip does, however if they
-- are too short to apply to the FPList an error is thrown.  
knitWith :: (a -> c -> e) -> (b -> d -> f) 
              -> [c] -> [d] -> FPList a b -> FPList e f
knitWith f g xs ys fpls = step xs ys fpls where
  step (c:_)  _      (FPList a End)           = singleton (f a c)
  step []     _      _                          = errshort "knitWith" " first"  
  step (c:cs) (d:ds) (FPList a (Next b rest)) = 
      cons (f a c) (g b d) rs where rs = step cs ds rest
  step _      []     _                          = errshort "knitWith" " second"
    


knitOnA :: (a -> c -> d) -> [c] -> FPList a b -> FPList d b
knitOnA f xs fpls = step xs fpls where
  step (c:_)  (FPList a End)            = singleton (f a c)
  step []     _                           = errshort "knitOnA" ""
  step (c:cs) (FPList a (Next b rest))  = 
      cons (f a c) b rs where rs = step cs rest
      
      

knitOnB :: (b -> c -> d) -> [c] -> FPList a b -> FPList a d
knitOnB g ys fpls = step ys fpls where
  step _  (FPList a End)                = singleton a
  step [] (FPList _ (Next _ _))         = errshort "knitOnB" ""
  step (c:cs) (FPList a (Next b rest))  = 
      cons a (g b c) rs where rs = step cs rest

errshort :: String -> String -> a
errshort t s = error $ t ++ " -" ++ s ++ " list too short"
-}

               