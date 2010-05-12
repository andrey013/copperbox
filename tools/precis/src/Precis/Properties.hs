{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Properties
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Precis.Properties
  (
  -- * Property type
    Property(..)
  
  -- * Edit type
  , Edit(..)

  -- * Edit operations
  , difference
  , diffProperty
  , addedRemoved
  , conflictRemoved
  , addedConflictRemoved

  ) where

import Precis.Utils

import Data.List ( find )


data Property n = Property 
      { property_name         :: String
      , property_description  :: String 
      , property_value        :: n
      }
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------

data Edit a = ADD a | DIF a a | EQU a | DEL a
  deriving (Eq,Show)

-- some edits are better as ADD | EQU | DEL though...


difference :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
difference matches conflict as bs = toListH $ checkShort bs (checkLong as id)
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



diffProperty :: (n -> n -> b) -> Property n -> Property n -> b
diffProperty cmp (Property _ _ a) (Property _ _ b) = cmp a b


addedRemoved :: [Edit a] -> ([a],[a])
addedRemoved = foldr fn ([],[])
  where
    fn (ADD a) (as,rs) = (a:as,rs)
    fn (DEL r) (as,rs) = (as,r:rs)
    fn _       acc     = acc

addedConflictRemoved :: [Edit a] -> ([a],[(a,a)],[a])
addedConflictRemoved = foldr fn ([],[],[])
  where
    fn (ADD a)   (as,cs,rs) = (a:as,cs,rs)
    fn (DIF a b) (as,cs,rs) = (as,(a,b):cs,rs)
    fn (DEL r)   (as,cs,rs) = (as,cs,r:rs)
    fn _         acc        = acc


conflictRemoved :: [Edit a] -> ([(a,a)],[a])
conflictRemoved = foldr fn ([],[])
  where
    fn (DIF a b)   (cs,rs) = ((a,b):cs,rs)
    fn (DEL r)     (cs,rs) = (cs,r:rs)
    fn _           acc     = acc

