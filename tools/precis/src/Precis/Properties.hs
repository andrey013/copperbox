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
  , summarizeAddedRemoved
  , summarizeConflictRemoved
  , summarizeAddedConflictRemoved

  ) where

import Precis.PPShowS
import Precis.Utils

import Data.List ( find )


data Property n = Property 
      { property_name         :: String
      , property_description  :: String 
      , property_value        :: n
      }
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------

data Edit a = Added a | Conflict a a | Same a | Removed a
  deriving (Eq,Show)



difference :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
difference matches conflict as bs = toListH $ checkShort bs (checkLong as id)
  where
    checkLong []     f = f
    checkLong (x:xs) f = case find (matches x) bs of
        Just b | conflict x b -> checkLong xs (f `snocH` Conflict x b) 
               | otherwise    -> checkLong xs (f `snocH` Same x)
        Nothing               -> checkLong xs (f `snocH` Added x)

    checkShort []     f = f
    checkShort (y:ys) f = case find (matches y) as of
        Just _                -> checkShort ys f   -- already found
        Nothing               -> checkShort ys (f `snocH` Removed y)



diffProperty :: (n -> n -> b) -> Property n -> Property n -> b
diffProperty cmp (Property _ _ a) (Property _ _ b) = cmp a b


addedRemoved :: [Edit a] -> ([a],[a])
addedRemoved = foldr fn ([],[])
  where
    fn (Added a)   (as,rs) = (a:as,rs)
    fn (Removed r) (as,rs) = (as,r:rs)
    fn _           acc     = acc

addedConflictRemoved :: [Edit a] -> ([a],[(a,a)],[a])
addedConflictRemoved = foldr fn ([],[],[])
  where
    fn (Added a)      (as,cs,rs) = (a:as,cs,rs)
    fn (Conflict a b) (as,cs,rs) = (as,(a,b):cs,rs)
    fn (Removed r)    (as,cs,rs) = (as,cs,r:rs)
    fn _              acc        = acc


conflictRemoved :: [Edit a] -> ([(a,a)],[a])
conflictRemoved = foldr fn ([],[])
  where
    fn (Conflict a b)   (cs,rs) = ((a,b):cs,rs)
    fn (Removed  r)     (cs,rs) = (cs,r:rs)
    fn _                acc     = acc


summarizeAddedRemoved :: String -> String -> (a -> String) -> [Edit a] -> ShowS
summarizeAddedRemoved single plural sf xs = 
           added_msg <> comma <+> removed_msg 
    `nextLine` vsep (map (addedLine sf)   as)
    `nextLine` vsep (map (removedLine sf) rs)
  where
    (as,rs)       = addedRemoved xs
    added_msg     = addedMsg    single plural (length as)
    removed_msg   = removedMsg  single plural (length rs)


summarizeConflictRemoved :: String -> String -> (a -> String) -> [Edit a] -> ShowS
summarizeConflictRemoved single plural sf xs = 
           conflict_msg <> comma <+> removed_msg 
    `nextLine` vsep (map (conflictLine sf) cs)
    `nextLine` vsep (map (removedLine sf)  rs)
  where
    (cs,rs)       = conflictRemoved xs
    conflict_msg  = conflictMsg single plural (length cs)
    removed_msg   = removedMsg  single plural (length rs)


summarizeAddedConflictRemoved :: String 
                              -> String 
                              -> (a -> String) -> [Edit a] -> ShowS
summarizeAddedConflictRemoved single plural sf xs = 
           added_msg <> comma <+> conflict_msg <> comma <+> removed_msg 
    `nextLine` vsep (map (addedLine sf)    as)
    `nextLine` vsep (map (conflictLine sf) cs)
    `nextLine` vsep (map (removedLine sf)  rs)
  where
    (as,cs,rs)    = addedConflictRemoved xs
    added_msg     = addedMsg    single plural (length as)
    conflict_msg  = conflictMsg single plural (length cs)
    removed_msg   = removedMsg  single plural (length rs)


-- 

msgCount :: String -> String -> Int -> ShowS
msgCount single _      1 = int 1 <+> text single
msgCount _      plural n = int n <+> text plural


addedMsg :: String -> String -> Int -> ShowS
addedMsg single plural i = msgCount single plural i <+> text "added (+)"

conflictMsg :: String -> String -> Int -> ShowS
conflictMsg single plural i = msgCount single plural i <+> text "conflict (*)"

removedMsg :: String -> String -> Int -> ShowS
removedMsg single plural i = msgCount single plural i <+> text "removed (-)"


addedLine :: (a -> String) -> a -> ShowS
addedLine f a = char '+' <+> (text $ f a)

conflictLine :: (a -> String) -> (a,a) -> ShowS
conflictLine f (a,b) = prefixLines (text "< ") (f a) `line`
                       prefixLines (text "> ") (f b) <> newline

removedLine :: (a -> String) -> a -> ShowS
removedLine f a      = char '-' <+> (text $ f a)
