{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions
--
--------------------------------------------------------------------------------

module Bala.Utils
  ( 
  -- * Various functions

  -- ** reverse application
    ( # )
  , iter
  
  , mapAfter
  , unfoldMap
  
  -- * Extra pretty printers
  , ( <^> )
  , vsepsep

  ) where

import Text.PrettyPrint.Leijen

import Data.List ( splitAt, foldl' )

-- Reverse application

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


-- | Apply the @f@ @n@ times to @a@. When @n<=0@ return a. 
iter :: Int -> (a -> a) -> a -> a
iter n f a | n <= 0    = a
           | otherwise = iter (n-1) f (f a)


mapAfter :: Int -> (a -> a) -> [a] -> [a]
mapAfter i f xs = ys ++ map f zs where
  (ys,zs) = splitAt i xs



-- | aka @anaMap@ in Bala
unfoldMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
unfoldMap _ s0 []     = ([],s0)     
unfoldMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = unfoldMap f st xs


--------------------------------------------------------------------------------
-- Extra pretty printers

infixr 5 <^>
(<^>) :: Doc -> Doc -> Doc 
(<^>) a b = a <$$> text "" <$$> b

vsepsep :: [Doc] -> Doc 
vsepsep []     = empty
vsepsep (x:xs) = foldl' (<^>) x xs
