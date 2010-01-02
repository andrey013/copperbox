{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint.JoinString
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Strings represented as /join lists/ for efficient append.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.JoinString
  ( 
    JoinString

  , toString
  , empty
  , text 
  , cons
  , snoc
  , cons1
  , (++)
  , length  
  , null
  , foldr
  , foldl
  , takeLeft  
  , takeRight

  ) where


import Data.Monoid


import qualified Prelude as Pre
import Prelude hiding ( (++), foldl, foldr, length, null )


data JoinString = Empty 
                | Leaf Int String 
                | Tree Int JoinString JoinString
  deriving (Eq,Show)

--------------------------------------------------------------------------------

instance Monoid JoinString where
  mempty  = Empty
  mappend = (++)


empty :: JoinString
empty = Empty

-- | Build a JoinString from a string
--
text :: String -> JoinString
text [] = Empty
text s  = Leaf (Pre.length s) s

cons :: String -> JoinString -> JoinString
cons s xs = text s ++ xs

snoc :: JoinString -> String -> JoinString
snoc xs s = xs ++ text s

-- | Character cons (potentially) efficient if the join string
-- is a leaf.
-- 
cons1 :: Char -> JoinString -> JoinString
cons1 c Empty        = Leaf 1 [c]
cons1 c (Leaf i s)   = Leaf (i+1) (c:s)
cons1 c (Tree i t u) = Tree (i+1) (cons1 c t) u


-- | Concatenate two join strings. Unlike (++) on regular lists, 
-- concatenation on join strings is (relatively) cheap hence the 
-- name /join list/.
--
(++) :: JoinString -> JoinString -> JoinString
Empty ++ ys    = ys
xs    ++ Empty = xs
xs    ++ ys    = Tree (length xs + length ys)  xs ys


length :: JoinString -> Int
length Empty        = 0
length (Leaf i _)   = i
length (Tree i _ _) = i

null :: JoinString -> Bool
null Empty = True
null _     = False


toString :: JoinString -> String
toString = foldr (flip (Pre.++)) ""


-- | Right-associative fold of a JoinString.
--
foldr :: (String -> b -> b) -> b -> JoinString -> b
foldr _ e Empty         = e
foldr f e (Leaf _ xs)   = f xs e
foldr f e (Tree _ t u)  = foldr f (foldr f e t) u


-- | Left-associative fold of a JoinString.
--
foldl :: (b -> String -> b) -> b -> JoinString -> b
foldl _ e Empty         = e
foldl f e (Leaf _ xs)   = f e xs
foldl f e (Tree _ t u)  = foldl f (foldl f e u) t

-- | 'takeLeft'  flattens the join-string.
--
takeLeft :: Int -> JoinString -> JoinString
takeLeft a = build . step a where
    build (i,xs) | i <= 0           = Empty
                 | otherwise        = Leaf i xs

    step :: Int -> JoinString -> (Int,String)
    step _ Empty                    = (0,"")
    step n (Leaf i xs)  | n >= i    = (i,xs) 
                        | otherwise = (n,Pre.take n xs)
    step n (Tree _ t u)             = let (i,ls) = step n t in
                                      if i<n then let (j,rs) = step (n-i) u in 
                                                  (i+j,ls Pre.++ rs)
                                             else (i,ls)
 
-- | 'takeRight'  flattens the join-string.
--
takeRight :: Int -> JoinString -> JoinString
takeRight a = build . step a where
    build (i,xs) | i <= 0           = Empty
                 | otherwise        = Leaf i xs

    step :: Int -> JoinString -> (Int,String)
    step _ Empty                    = (0,"")
    step n (Leaf i xs)  | n >= i    = (i,xs) 
                        | otherwise = (n,ltr n xs)
    step n (Tree _ t u)             = let (i,rs) = step n u in
                                      if i<n then let (j,ls) = step (n-i) t in 
                                                  (i+j,ls Pre.++ rs)
                                             else (i,rs)
 
 


ltr :: Int -> [a] -> [a]
ltr n = ($ []) . snd . Pre.foldr fn (0,id) where
  fn e (i,f) | i < n      = (i+1, (e:) . f)
             | otherwise  = (i,f)


