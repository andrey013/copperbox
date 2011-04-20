{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.HSStream
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Head strict stream of Double
--
--------------------------------------------------------------------------------


module ZSyn.HSStream
  ( 
  -- * Stream
    HSStream(..)
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

import Prelude hiding ( head, tail, repeat, map, zip, take, drop
                      , iterate, cycle, sum, const )

--------------------------------------------------------------------------------


-- | Head strict stream.
--
data HSStream a = !a :< HSStream a 
  deriving Ord

instance Eq a => Eq (HSStream a) where
  (a :< s) /= (b :< t) = if a /= b then (/=) s t else False
  (a :< s) == (b :< t) = if a /= b then False else (==) s t


instance Show a => Show (HSStream a) where
  showsPrec n s = showsPrec n (take 8 s) . showString " ..." 



infixr 5 :<

head :: (HSStream a) -> a
head (a :< _) = a

tail :: HSStream a -> HSStream a
tail (_ :< s) = s

repeat :: a -> HSStream a
repeat a = a :< repeat a

map :: (a -> b) -> HSStream a -> HSStream b
map f (a :< s) = f a :< map f s 

zip :: (a -> b -> c) -> HSStream a -> HSStream b -> HSStream c
zip f sx sy = step sx sy 
  where
    step (a :< sa) (b :< sb) = f a b :< step sa sb  





instance Num a => Num (HSStream a) where
  (+)           = zip (+)
  (-)           = zip (-) 
  (*)           = zip (*)
  abs           = map abs
  negate        = map negate
  signum        = map signum
  fromInteger i = repeat (fromInteger i)

iterate :: (a -> a) -> a -> HSStream a
iterate f a = a :< iterate f (f a)

infixr 5 <<

(<<) :: [a] -> HSStream a -> HSStream a
[]     << s = s
(x:xs) << s = x :< (xs << s)

diff :: Num a => HSStream a -> HSStream a
diff s = tail s - s

sum :: Num a => HSStream a -> HSStream a
sum s = t where t = 0 :< t + s


const :: Num a => a -> HSStream a
const n = n :< repeat 0


(*>) :: Num a => a -> HSStream a -> HSStream a
(*>) x s = map (x*) s

merge :: HSStream a -> HSStream a -> HSStream a
merge (a :< sa) (b :< sb) = a :< b :< merge sa sb

cycle :: [a] -> HSStream a
cycle xs = foldr (:<) (cycle xs) xs


take :: Int -> HSStream a -> [a]
take n _          | n <= 0 = []
take n (a :< sa)           = a : take (n-1) sa




(>*<) :: Num a => HSStream a -> HSStream a -> HSStream a
(>*<) (b0 :< bq) a@(a0 :< aq) = b0 * a0 :< aq + bq >*< a


(>/<) :: Fractional a => HSStream a -> HSStream a -> HSStream a
(>/<) (b0 :< bq) a@(a0 :< aq) = w0 :< (bq - w0 *> aq) >/< a
  where
    w0 = b0 / a0