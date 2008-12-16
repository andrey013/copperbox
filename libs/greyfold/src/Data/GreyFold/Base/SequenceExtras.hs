{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.SequenceExtras
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Data.Sequence versions of functions from Data.List 
--
--------------------------------------------------------------------------------

module GreyFold.Base.SequenceExtras where


import qualified Data.Foldable as F
import Data.Sequence
import Prelude hiding ( null, span, replicate, zipWith )


foldl :: (b -> a -> b) -> b -> Seq a -> b
foldl = F.foldl

foldr :: (a -> b -> b) -> b -> Seq a -> b
foldr = F.foldr

zip :: Seq a -> Seq b -> Seq (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f sx sy = step (viewl sx) (viewl sy) where
    step (a :< sa) (b :< sb)  = (f a b) <| step (viewl sa) (viewl sb)
    step _    _               = empty


    
replicate :: Int -> a -> Seq a
replicate i a | i <= 0     = empty
              | otherwise  = a <| replicate (i-1) a 
  
maximum :: (Ord a) => (Seq a) -> a
maximum se | null se = error "smaximum: empty sequence"
           | otherwise = F.foldr max a sa where (a :< sa) = viewl se  

minimum :: (Ord a) => (Seq a) -> a
minimum se | null se = error "minimum: empty sequence"
           | otherwise = F.foldr min a sa where (a :< sa) = viewl se 

concat :: Seq (Seq a) -> Seq a
concat = F.foldr (><) empty




transpose :: Seq (Seq a) -> Seq (Seq a)
transpose = step . viewl
  where
    step EmptyL         = empty
    step (x :< sse)     = case viewl x of
        EmptyL    -> transpose sse
        (e :< se) -> (e <| allheads sse) <| (transpose $ se <| alltails sse)
    
    allheads = F.foldl' (\acc se -> acc |> head1 se) empty
    alltails = F.foldl' (\acc se -> acc |> tail1 se) empty
    
    head1 se = case viewl se of EmptyL -> unmatchErr; 
                                (a :< _) -> a
    
    tail1 se = case viewl se of EmptyL -> unmatchErr; 
                                (_ :< sa) -> sa
                                
    unmatchErr = error "stranspose - sequences of unequal length" 
    
filter :: (a -> Bool) -> Seq a -> Seq a
filter pf = step . viewl where
  step EmptyL     = empty
  step (a :< sa) | pf a       = a <| step (viewl sa)
                 | otherwise  = step (viewl sa)

groupBy                 :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy eq se           = step (viewl se) where
    step EmptyL         = empty
    step (a :< sa)      = (a <| sy) <| step (viewl sz) where
                            (sy,sz) = span (eq a) sa


span                    :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
span p se               = step (viewl se) where
    step EmptyL         = (empty,empty)
    step (a :< sa)      
            | p a       = let (sy,sz) = step (viewl sa) in (a <| sy, sz)
            | otherwise = (empty, a <| sa)                             

 


