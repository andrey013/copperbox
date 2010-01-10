{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.Perm
-- Copyright   :  (c) Stephen Tetley 2010 (derived from Daan Leijen (c) 1999-2001)
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Acknowledgment - the code here originated from Daan Leijen\'s
-- Parsec, which in turn originated from Arthur Baars, 
-- Andres L\"oh, and S. Doaitse Swierstra's paper \"Parsing 
-- Permutation Phrases.\"
--
-- The changes from Parsec are: 
-- 1. Changing the function types so that the only type class 
--    obligation is to Alternative.
-- 2. Renaming the data types to be closer to the ones in the 
--    original paper.
-- 3. A little bit of golf and Functor instances rather than 
--    mapPerms and mapBranch.
-- 
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ZParse.Perm
  ( 
    Perms
  , permute
  , (<||>), (<$$>)
  , (<|?>), (<$?>)  
  ) where

import Control.Applicative

data Perms p a = Choice { _defaults :: Maybe a, _branches :: [Branch p a] }

data Branch p a = forall x . Branch (Perms p (x -> a)) (p x)


choice1 :: Alternative f => [f a] -> f a -> f a
choice1 fs last_choice = foldr (<|>) last_choice fs

instance Functor (Perms p) where
  fmap f (Choice x xs) = Choice (fmap f x) (map (fmap f) xs)
 
instance Functor (Branch p) where
  fmap f (Branch perm p) = Branch (fmap (f .) perm) p

--------------------------------------------------------------------------------

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>


(<||>) :: Perms p (a -> b) -> p a -> Perms p b
(<||>) = add

(<$$>) :: (a -> b) -> p a -> Perms p b
(<$$>) f p        = newperm f <||> p

(<|?>) :: Perms p (a -> b) -> (a, p a) -> Perms p b
(<|?>) perm (x,p) = addopt perm x p

(<$?>) :: (a -> b) -> (a, p a) -> Perms p b
(<$?>) f (x,p)    = newperm f <|?> (x,p)

permute :: Alternative f => Perms f a -> f a
permute (Choice def xs) = choice1 (map branch xs) default_choice
  where
    default_choice          = maybe empty pure def
    branch (Branch perm p)  = (flip ($)) <$> p <*> permute perm


-- build permutation trees
newperm :: (a -> b) -> Perms p (a -> b)
newperm f = Choice (Just f) []

add :: Perms p (a -> b) -> p a -> Perms p b
add perm@(Choice _mf fs) p = Choice Nothing (first:map insert fs)
  where
    first                     = Branch perm p
    insert (Branch perm' p')  = Branch (add (fmap flip perm') p) p'


addopt :: Perms p (a -> b) -> a -> p a -> Perms p b 
addopt perm@(Choice mf fs) x p = Choice (fmap ($ x) mf) (first:map insert fs)
  where
    first                     = Branch perm p
    insert (Branch perm' p')  = Branch (addopt (fmap flip perm') x p) p'
