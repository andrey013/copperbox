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
    fn (Removed a) (as,rs) = (as,a:rs)
    fn _           acc     = acc

conflictRemoved :: [Edit a] -> ([(a,a)],[a])
conflictRemoved = foldr fn ([],[])
  where
    fn (Conflict a b)   (cs,rs) = ((a,b):cs,rs)
    fn (Removed  a)     (cs,rs) = (cs,a:rs)
    fn _                acc     = acc


summarizeAddedRemoved :: String -> String -> (a -> String) -> [Edit a] -> ShowS
summarizeAddedRemoved single plural str xs = 
           added_msg <> comma <+> removed_msg 
    `line` vsep (map added1   as)
    `line` vsep (map removed1 rs)
  where
    (as,rs)     = addedRemoved xs
    (alen,rlen) = (length as, length rs)
    countfun    = msgCount single plural

    added_msg   = countfun alen <+> text "added (+)"
    removed_msg = countfun rlen <+> text "removed (-)"
 
    added1 a     = char '+' <+> (text $ str a)
    removed1 a   = char '-' <+> (text $ str a)

msgCount :: String -> String -> Int -> ShowS
msgCount single _      1 = int 1 <+> text single
msgCount _      plural n = int n <+> text plural

summarizeConflictRemoved :: String -> String -> (a -> String) -> [Edit a] -> ShowS
summarizeConflictRemoved single plural str xs = 
           conflict_msg <> comma <+> removed_msg 
    `line` vsep (map conflict1 cs)
    `line` vsep (map removed1  rs)
  where
    (cs,rs)       = conflictRemoved xs
    (clen,rlen)   = (length cs, length rs)
    countfun      = msgCount single plural

    conflict_msg  = countfun clen <+> text "conflict (*)"
    removed_msg   = countfun rlen <+> text "removed (-)"
 
    conflict1 (a,b) = prefixLines (text "< ") (str a) `line`
                      prefixLines (text "> ") (str b) <> newline
    removed1 a      = char '-' <+> (text $ str a)
