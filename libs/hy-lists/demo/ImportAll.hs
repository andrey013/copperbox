


module ImportAll where

import qualified Data.Hy.OneList  as One
import Data.Hy.SnocList ( SnocList(..) )
import qualified Data.Hy.SnocList as Snoc
import Data.Hy.Hylomorphisms

import qualified Data.List as List


filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr pred = foldr fn [] where
  fn a xs | pred a    = a:xs
          | otherwise = xs


filter_unfoldr'bad :: (a -> Bool) -> [a] -> [a]
filter_unfoldr'bad pred = List.unfoldr phi where
   phi (a:xs) | pred a = Just (a,xs)
      -- The result of the next case is clearly wrong 
      -- @Just (a,xs)@, but we must be productive with
      -- an unfold.
   phi (a:xs)          = Just (a,xs)
   phi []              = Nothing

list_de :: [a] -> Maybe (a,[a])
list_de (x:xs) = Just (x,xs)
list_de []     = Nothing


list_filter_co :: (a -> Bool) -> a -> [a] -> [a]
list_filter_co pf a xs | pf a      = a:xs
                       | otherwise = xs



snoc_de :: SnocList a -> Maybe (a, SnocList a)
snoc_de (xs :> x) = Just (x,xs)
snoc_de Lin       = Nothing


-- Nothing special on the unfold step all the work in 
-- the rebuilding (the fold).
filter_hylor :: (a -> Bool) -> [a] -> [a]
filter_hylor pf = hylor list_de (list_filter_co pf) [] where


snoc_nums :: SnocList Int
snoc_nums = Lin :> 1 :> 2 :> 3 :> 4 :> 5 :> 6


-- This is reversed... 
filter_hylo_snoc pf = hylol snoc_de (flip $ list_filter_co pf)[]


reverse_hylor :: [a] -> [a]
reverse_hylor = hylol list_de (flip (:)) []


foldr_reverse'not = foldr (:) []
 
foldl_reverse = foldl (flip (:)) []




