{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Diff
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Two Diff types (3 state and 4 state)
--
--------------------------------------------------------------------------------


module Precis.Diff
  (
  
  -- * Edit types
    Edit4(..)
  , Edit3(..)

  -- * Edit operations
  , diff4
  , diff3

  , addedRemoved
  , conflictRemoved
  , addedConflictRemoved

  ) where

import Precis.Utils.Common

import Data.List ( find )

--------------------------------------------------------------------------------

data Edit4 a = ADD a | DIF a a | EQU a | DEL a
  deriving (Eq,Show)

-- some edits are better as ADD | EQU | DEL though...

data Edit3 a = Add a | Equ a | Del a
  deriving (Eq,Show)


instance Functor Edit4 where
  fmap f (ADD a)   = ADD (f a)
  fmap f (DIF a b) = DIF (f a) (f b)
  fmap f (EQU a)   = EQU (f a)
  fmap f (DEL a)   = DEL (f a)

instance Functor Edit3 where
  fmap f (Add a)   = Add (f a)
  fmap f (Equ a)   = Equ (f a)
  fmap f (Del a)   = Del (f a)




diff4 :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a] -> [Edit4 a]
diff4 matches conflict as bs = toListH $ checkShort bs (checkLong as id)
  where
    checkLong []     f = f
    checkLong (x:xs) f = case find (matches x) bs of
        Just b | conflict x b -> checkLong xs (f `snocH` DIF x b) 
               | otherwise    -> checkLong xs (f `snocH` EQU x)
        Nothing               -> checkLong xs (f `snocH` ADD x)

    checkShort []     f = f
    checkShort (y:ys) f = case find (matches y) as of
        Just _                -> checkShort ys f   -- already found
        Nothing               -> checkShort ys (f `snocH` DEL y)


diff3 :: (a -> a -> Bool) -> [a] -> [a] -> [Edit3 a]
diff3 matches as bs = toListH $ checkShort bs (checkLong as id)
  where
    checkLong []     f = f
    checkLong (x:xs) f = case find (matches x) bs of
        Just _    -> checkLong xs (f `snocH` Equ x)
        Nothing   -> checkLong xs (f `snocH` Add x) 

    checkShort []     f = f
    checkShort (y:ys) f = case find (matches y) as of
        Just _    -> checkShort ys f          -- already found
        Nothing   -> checkShort ys (f `snocH` Del y) 


addedRemoved :: [Edit4 a] -> ([a],[a])
addedRemoved = foldr fn ([],[])
  where
    fn (ADD a) (as,rs) = (a:as,rs)
    fn (DEL r) (as,rs) = (as,r:rs)
    fn _       acc     = acc

addedConflictRemoved :: [Edit4 a] -> ([a],[(a,a)],[a])
addedConflictRemoved = foldr fn ([],[],[])
  where
    fn (ADD a)   (as,cs,rs) = (a:as,cs,rs)
    fn (DIF a b) (as,cs,rs) = (as,(a,b):cs,rs)
    fn (DEL r)   (as,cs,rs) = (as,cs,r:rs)
    fn _         acc        = acc


conflictRemoved :: [Edit4 a] -> ([(a,a)],[a])
conflictRemoved = foldr fn ([],[])
  where
    fn (DIF a b)   (cs,rs) = ((a,b):cs,rs)
    fn (DEL r)     (cs,rs) = (cs,r:rs)
    fn _           acc     = acc

