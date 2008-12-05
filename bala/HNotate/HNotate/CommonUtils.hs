
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- Common utils - recursion schemes for Data.Sequences 
-- and pretty print functions
--
--------------------------------------------------------------------------------

module HNotate.CommonUtils where

-- Should have no dependencies on other HNotate modules

import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Sequence hiding (empty, length, reverse, update)
import qualified Data.Sequence as S
import Prelude hiding (null)


--------------------------------------------------------------------------------
-- HOF's

-- Reverse application and composition

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> (a -> c) 
g #. f = f . g


-- variations of 'on' 

onr :: (a -> c -> d) -> (b -> c) -> a -> b -> d
op `onr` f = \x y -> x `op` f y

onl :: (c -> b -> d) -> (a -> c) -> a -> b -> d
op `onl` f = \x y -> f x `op` y

foldlOn :: F.Foldable t => (a -> b -> a) -> (c -> b) -> a -> t c -> a
foldlOn op ppf = F.foldl (op `onr` ppf)

foldrOn :: F.Foldable t => (b -> a -> a) -> (c -> b) -> a -> t c -> a
foldrOn op ppf = F.foldr (op `onl` ppf)




--------------------------------------------------------------------------------
-- variations on either - postprocessing with a success or failure continuation

eitherSk :: (b -> c) -> Either a b -> Either a c
eitherSk sk = either (Left . id)  (Right . sk)

eitherSkM :: Monad m => (b -> m c) -> Either a b -> m (Either a c)
eitherSkM sk = either (return . Left . id)  (\a -> sk a >>= return . Right)

eitherSkM' :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
eitherSkM' sk = either (return . Left . id)  (\a -> sk a >>= return)



-- pairs
fork :: (a -> b) -> (a,a) -> (b,b)
fork f (a,b) = (f a, f b)

forkM :: Monad m => (a -> m b) -> (a,a) -> m (b,b)
forkM f (a,b) = f a >>= \a' -> f b >>= \b' -> return (a',b')


prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

prodM :: Monad m => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
prodM f g (a,b) = f a >>= \a' -> g b >>= \b' -> return (a',b') 

dup :: a -> (a,a)
dup a = (a,a)




--------------------------------------------------------------------------------
-- enum functions for cycles (primarily helpful for pitch letters)


enumFromCyc :: (Bounded a, Enum a, Eq a) => a -> [a]
enumFromCyc a = a : (unfoldr f $ nextOf a)
  where 
    f x | x == a    = Nothing
        | otherwise = Just (x,nextOf x)
    
enumFromToCyc :: (Bounded a, Enum a, Eq a) => a -> a -> [a]
enumFromToCyc a b | a == b    = [a]
                  | otherwise = a : (unfoldr f $ nextOf a) ++ [b]
  where 
    f x | x == a || x == b   = Nothing
        | otherwise          = Just (x,nextOf x)

nextOf :: (Bounded a, Eq a, Enum a) => a -> a  
nextOf x | x == maxBound = minBound
         | otherwise     = succ x



--------------------------------------------------------------------------------
-- ShowS helpers

spaceS :: Int -> ShowS
spaceS i = showString (replicate i ' ')  

showSpace :: ShowS
showSpace = showChar ' '     
    
showNewline, showLBrace, showRBrace :: ShowS
showNewline   = showChar '\n'
showLBrace    = showString "{\n"
showRBrace    = showString "}\n"

foldS :: (ShowS -> ShowS -> ShowS) -> [ShowS] -> ShowS
foldS _ []      = id
foldS f xs      = foldr1 f xs

constrS :: String -> ShowS -> ShowS
constrS cname body = showString ('(':cname) . showSpace . body . showChar ')'


-- to sort out...
showsPrecChain2 :: (Show a, Show b) => Int -> a -> b -> ShowS
showsPrecChain2 i a b = showsPrec i a . showSpace . showsPrec i b 

showsPrecChain3 :: (Show a, Show b, Show c) => Int -> a -> b -> c -> ShowS
showsPrecChain3 i a b c = 
    showsPrec i a . showSpace . showsPrec i b . showSpace . showsPrec i c
