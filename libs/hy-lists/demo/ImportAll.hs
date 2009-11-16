


module ImportAll where

import Data.Hy.OneList                  ( OneList(..) )
import qualified Data.Hy.OneList        as One
import Data.Hy.SnocList                 ( SnocList(..) )
import qualified Data.Hy.SnocList       as Snoc
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




snoc_de :: SnocList a -> Maybe (a, SnocList a)
snoc_de (xs :> x) = Just (x,xs)
snoc_de Lin       = Nothing


-- Nothing special on the unfold step all the work in 
-- the rebuilding (the fold).
filter_hylor :: (a -> Bool) -> [a] -> [a]
filter_hylor pf = hylor list_de (list_filter_cons pf) [] where


snoc_nums :: SnocList Int
snoc_nums = Lin :> 1 :> 2 :> 3 :> 4 :> 5 :> 6


ones_nums :: OneList Int
ones_nums = 1 :+ 2 :+ 3 :+ 4 :+ One 5

-- This is reversed... 
filter_hylo_snoc pf = hylol snoc_de (flip $ list_filter_cons pf)[]


reverse_hylor :: [a] -> [a]
reverse_hylor = hylol list_de (flip (:)) []


foldr_reverse'not = foldr (:) []
 
foldl_reverse = foldl (flip (:)) []

hylo_mapM :: (a -> m a) -> OneList a -> m [a]
hylo_mapM = undefined

-- type Zero12 a b = Zero | One a | Two b

onelist_de :: OneList a -> Either a (a, OneList a)
onelist_de (One a)   = Left a
onelist_de (a :+ xs) = Right (a,xs)


hylo_OneListToList = onehylor onelist_de (:) [] 



takeWhileList pf = hylor (list_while_des pf) (:) []


-- whoops reveresed!
dropWhileList pf = snd . hylol list_des (flip $ list_drop_cons pf) (True,[])