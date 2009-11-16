{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Hy.Hylomorphisms
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
-- Hylomorphisms, destructors, constructors ...
--
--------------------------------------------------------------------------------

module Data.Hy.Hylomorphisms
  ( 
  
  -- * Hylomorphisms
    hylor
  , hylol

  , onehylor
  , onehylol

  -- * Destructors
  , list_des
  , list_while_des 

  -- * Constructors
  , list_cons
  , list_filter_cons
  , list_drop_cons
  
  ) where


-- | @hylor@ - a right fold after an unfold. 
hylor :: (st -> Maybe (a,st)) -> (a -> b -> b) -> b -> st -> b
hylor g f e a = case g a of
                  Nothing     -> e
                  Just (b,st) -> f b (hylor g f e st)


-- | @hylol@ - a left fold after an unfold.
hylol :: (st -> Maybe (a,st)) -> (b -> a -> b) -> b -> st -> b
hylol g f e a = case g a of
                  Nothing     -> e
                  Just (b,st) -> hylol g f (f e b) st




-- | @onehylor@ - a right fold after an unfold for a OneList or
-- similar non-empty container. 
onehylor :: (st -> Either a (a,st)) -> (a -> b -> b) -> b -> st -> b
onehylor g f e a = case g a of
                     Left b       -> f b e
                     Right (b,st) -> f b (onehylor g f e st)


-- | @onehylol@ - a left fold after an unfold for a OneList or 
-- similar non-empty container.
onehylol :: (st -> Either a (a,st)) -> (b -> a -> b) -> b -> st -> b
onehylol g f e a = case g a of
                  Left b       -> f e b
                  Right (b,st) -> onehylol g f (f e b) st


--------------------------------------------------------------------------------
-- Destructors

-- | Destructor for lists
list_des :: [a] -> Maybe (a,[a])
list_des (x:xs) = Just (x,xs)
list_des []     = Nothing

list_while_des :: (a -> Bool) -> [a] -> Maybe (a,[a])
list_while_des pf (x:xs) | pf x      = Just (x,xs)
list_while_des _  _                  = Nothing



--------------------------------------------------------------------------------
-- Constructors

list_cons :: a -> [a] -> [a]
list_cons = (:)


list_filter_cons :: (a -> Bool) -> a -> [a] -> [a]
list_filter_cons pf a xs | pf a      = a:xs
                         | otherwise = xs


list_drop_cons :: (a -> Bool) -> a -> (Bool,[a]) -> (Bool,[a])
list_drop_cons pf a (True,xs) | pf a = (True,xs)
list_drop_cons _  a (_,xs)           = (False,a:xs)

