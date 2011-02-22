{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Syn.Stream
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Head strict stream
--
--------------------------------------------------------------------------------


module Syn.Stream
  ( 
  -- * Stream
    Stream(..)
  , head
  , tail
  , repeat
  , map
  , zip

  , iterate
  , (<<)
  , diff
  , sum
  , const

  , (*>)
  , merge
  , cycle
  , take

  , (>*<)
  , (>/<)

  ) where 

import Prelude ( Show(..), showString, Eq(..), Ord(..), Num(..), 
                 Fractional(..),
                 foldr, Int, (<=), (-), (.) )

--------------------------------------------------------------------------------


data Stream a = !a :< (Stream a)        deriving (Eq,Ord)

infixr 5 :<

head :: Stream a -> a
head (a :< _) = a

tail :: Stream a -> Stream a
tail (_ :< s) = s

repeat :: a -> Stream a
repeat a = a :< repeat a

map :: (a -> b) -> Stream a -> Stream b
map f (a :< s) = f a :< map f s 

zip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zip f sx sy = step sx sy 
  where
    step (a :< sa) (b :< sb) = f a b :< step sa sb  





instance Num a => Num (Stream a) where
  (+)           = zip (+)
  (-)           = zip (-) 
  (*)           = zip (*)
  abs           = map abs
  negate        = map negate
  signum        = map signum
  fromInteger i = repeat (fromInteger i)

iterate :: (a -> a) -> a -> Stream a
iterate f a = a :< iterate f (f a)

infixr 5 <<

(<<) :: [a] -> Stream a -> Stream a
[]     << s = s
(x:xs) << s = x :< (xs << s)

diff :: Num a => Stream a -> Stream a
diff s = tail s - s

sum :: Num a => Stream a -> Stream a
sum s = t where t = 0 :< t + s


const :: Num a => a -> Stream a
const n = n :< repeat 0


(*>) :: Num a => a -> Stream a -> Stream a
(*>) x s = map (x*) s

merge :: Stream a -> Stream a -> Stream a
merge (a :< sa) (b :< sb) = a :< b :< merge sa sb

cycle :: [a] -> Stream a
cycle xs = foldr (:<) (cycle xs) xs


take :: Int -> Stream a -> [a]
take n _          | n <= 0 = []
take n (a :< sa)           = a : take (n-1) sa


instance Show a => Show (Stream a) where
  showsPrec n s = showsPrec n (take 8 s) . showString " ..." 


(>*<) :: Num a => Stream a -> Stream a -> Stream a
(>*<) (b0 :< bq) a@(a0 :< aq) = b0 * a0 :< aq + bq >*< a


(>/<) :: Fractional a => Stream a -> Stream a -> Stream a
(>/<) (b0 :< bq) a@(a0 :< aq) = w0 :< (bq - w0 *> aq) >/< a
  where
    w0 = b0 / a0