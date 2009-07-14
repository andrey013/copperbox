{-# OPTIONS -Wall #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OptionOM
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- A fence-post-list datatype. It is polymorphic on two types and 
-- statically ensures that the inner type is correctly bookended
-- and interspersed with the outer type. For example, it generates
-- a, aba, ababa, abababa, etc.  
--
--------------------------------------------------------------------------------

module Data.FPList  where

import Prelude hiding (length)

-- The main data type is a 'fence post list' - a list with two 
-- elements types. There is a static guarantee that there is one 
-- more outer element (fence-post) than inner element (fence-section).
-- eg. a, aba, ababa, etc. 
-- Note, that there is no way to construct an empty FPList, the 
-- smallest list must be a singleton.
data FPList a b = FPList a (FPList' a b)
  deriving (Show)
  
data FPList' a b = FPEnd 
                 | FPNext { _hole :: b, 
                            _rest :: FPList a b }
  deriving (Show)       
  
length :: FPList a b -> Int
length (FPList _ FPEnd)           = 1
length (FPList _ (FPNext _ rest)) = 2 + length rest 

singleton :: a -> FPList a b
singleton a = FPList a FPEnd

-- You can only extend a /fence-post-list/ with both an @a@ and a @b@. 
dblcons :: a -> b -> FPList a b -> FPList a b
dblcons a b rest = FPList a (FPNext b rest)

-- Build a fence-post-list with a prefix of @a@ and a list of @(b,a)@ pairs 
zipbuild :: a -> [(b,a)] -> FPList a b
zipbuild a []           = singleton a
zipbuild a ((b,a'):xs)  = dblcons a b rest where rest = zipbuild a' xs


merge :: (a -> c) -> (b -> c) -> FPList a b -> [c]
merge f _ (FPList a FPEnd)            = [f a]
merge f g (FPList a (FPNext b rest))  = f a : g b : merge f g rest


-- @bimerge@ is similar to the apomorphism recursion scheme.
-- It merges pair-wise up to the last @a@ element which it has 
-- to /flush/. 
bimerge :: (a -> b -> c) -> (a -> c) -> FPList a b -> [c]
bimerge _ apo (FPList a FPEnd)            = [apo a]
bimerge f apo (FPList a (FPNext b rest))  = f a b : bimerge f apo rest


data FPView a b = Sgl a
                | DCons a b (FPList a b)
  deriving (Show)              

fpview :: FPList a b -> FPView a b  
fpview (FPList a FPEnd)           = Sgl a
fpview (FPList a (FPNext b rest)) = DCons a b rest

-- @knitWith@ truncates the normal lists like zip does, however if they
-- are too short to apply to the FPList an error is thrown.  
knitWith :: (a -> c -> e) -> (b -> d -> f) 
              -> [c] -> [d] -> FPList a b -> FPList e f
knitWith f g xs ys fpls = step xs ys fpls where
  step (c:_)  _      (FPList a FPEnd)           = singleton (f a c)
  step []     _      _                          = errshort "knitWith" " first"  
  step (c:cs) (d:ds) (FPList a (FPNext b rest)) = 
      dblcons (f a c) (g b d) rs where rs = step cs ds rest
  step _      []     _                          = errshort "knitWith" " second"
    


knitOnA :: (a -> c -> d) -> [c] -> FPList a b -> FPList d b
knitOnA f xs fpls = step xs fpls where
  step (c:_)  (FPList a FPEnd)            = singleton (f a c)
  step []     _                           = errshort "knitOnA" ""
  step (c:cs) (FPList a (FPNext b rest))  = 
      dblcons (f a c) b rs where rs = step cs rest
      
      

knitOnB :: (b -> c -> d) -> [c] -> FPList a b -> FPList a d
knitOnB g ys fpls = step ys fpls where
  step _  (FPList a FPEnd)                = singleton a
  step [] (FPList _ (FPNext _ _))         = errshort "knitOnB" ""
  step (c:cs) (FPList a (FPNext b rest))  = 
      dblcons a (g b c) rs where rs = step cs rest

errshort :: String -> String -> a
errshort t s = error $ t ++ " -" ++ s ++ " list too short"
               