{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions
--
--------------------------------------------------------------------------------

module Bala.Core.Utils
  ( 
  -- * Various functions

    ( # )
  , iter
  
  , mapAfter
  , unfoldMap
  , matchZipWith
  , remZipWith
  
  -- * Extra pretty printers
  , ( <^> )
  , vsepsep
  , ( <$*> )

  -- * Specs!  
  , oo
  , ooo 
  , oooo

  -- * Pairs
  , swap 
  , fork
  , prod
  , prod'
  , dup
  , dist


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



matchZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
matchZipWith _ []     []     = []
matchZipWith f (x:xs) (y:ys) = f x y : matchZipWith f xs ys
matchZipWith _ _      _      = error "matchZipWith - unmatched lists"

-- | zipWith that returns the remainer of the second list on 
-- exhausting the first.
remZipWith :: (a -> b -> c) -> [a] -> [b] -> ([c],[b])
remZipWith _ []     ys     = ([],ys)
remZipWith f (x:xs) (y:ys) = (f x y:zs,ys') where (zs,ys') = remZipWith f xs ys
remZipWith _ _      []     = error "remZipWith - first list longer than second"


--------------------------------------------------------------------------------
-- Extra pretty printers

infixr 5 <^>
(<^>) :: Doc -> Doc -> Doc 
(<^>) a b = a <$$> text "" <$$> b

vsepsep :: [Doc] -> Doc 
vsepsep []     = empty
vsepsep (x:xs) = foldl' (<^>) x xs

infixr 6 <$*>
(<$*>) :: Doc -> Maybe Doc -> Doc 
(<$*>) a ob = maybe a (a <$>) ob



--------------------------------------------------------------------------------
-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g    

--------------------------------------------------------------------------------
-- Pairs


swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) a = (f a,g a)

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

prod' :: (a -> c, b -> d) -> (a,b) -> (c,d)
prod' (f,g) (a,b) = (f a, g b)

-- exl and exr are fst and snd respectively so are not defined here.

dup :: a -> (a,a)
dup a = (a,a)

dist :: (a -> b) -> (a,a) -> (b,b)
dist = (uncurry prod) . dup
