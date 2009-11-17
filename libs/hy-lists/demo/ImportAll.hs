


module ImportAll where

import Data.Hy.OneList                  ( OneList(..), onelist_des )
import qualified Data.Hy.OneList        as One
import Data.Hy.SnocList                 ( SnocList(..) )
import qualified Data.Hy.SnocList       as Snoc
import Data.Hy.Hylomorphisms

import Control.Functor                  -- category-extras

import Control.Monad ( ap, liftM, liftM2 )
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

hylo_mapM :: Monad m => (a -> m b) -> OneList a -> m [b]
hylo_mapM f = 
    onehylorM (return . onelist_des) 
              (appro (liftM2 (:)) f return) [] 

hylo_map :: (a -> b) -> OneList a -> [b]
hylo_map f = onehylor (onelist_des) (appro (:) f id)   [] 
  
-- The constructor is easily done with appor from Data.Aviary
--  fn = appro (:) f id

appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro f g h x y = f (g x) (h y) 


-- type Zero12 a b = Zero | One a | Two b


-- can this be defined by composition?
onelist_map_des :: (a -> b) -> OneList a -> Either b (b, OneList a)
onelist_map_des f = bimap f (bimap f id) . onelist_des

onelist_mapM_des :: Monad m 
                => (a -> m b) -> OneList a -> m (Either b (b, OneList a))
onelist_mapM_des f = bimapM f (bimapM f return) . onelist_des


hylo_OneListToList = onehylor onelist_des (:) [] 



takeWhileList pf = hylor (list_while_des pf) (:) []


-- whoops reveresed!
dropWhileList pf = snd . hylol list_des (flip $ list_drop_cons pf) (True,[])

bimappair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
bimappair f g (x,y) = (f x, g y)


-- | Bimap for 'Either'.
bimapeither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapeither f _ (Left x)  = Left (f x)
bimapeither _ g (Right y) = Right (g y)


bimappairM :: Monad m 
           => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
           
bimappairM f g (x,y) = return (,) `ap` f x `ap` g y


-- | Bimap for 'Either'.
bimapeitherM :: Monad m 
             => (a -> m c) -> (b -> m d) -> Either a b -> m (Either c d)
bimapeitherM f _ (Left x)  = f x >>= return . Left 
bimapeitherM _ g (Right y) = g y >>= return . Right
