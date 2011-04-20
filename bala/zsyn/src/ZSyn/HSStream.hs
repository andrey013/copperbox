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
-- Head strict stream.
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
  , prefix
  , drop

  , (>*<)
  , (>/<)

  , duplicate
  , skip


  ) where 

import Prelude hiding ( head, tail, repeat, map, zip, take, drop
                      , iterate, cycle, sum, const )

--------------------------------------------------------------------------------


-- | Head strict stream.
-- 
-- Note the stream has a phantom param.
--
data HSStream p a = !a :< HSStream p a 
  deriving Ord

instance Eq a => Eq (HSStream p a) where
  (a :< s) /= (b :< t) = if a /= b then (/=) s t else False
  (a :< s) == (b :< t) = if a /= b then False else (==) s t


instance Show a => Show (HSStream p a) where
  showsPrec n s = showsPrec n (take 8 s) . showString " ..." 



infixr 5 :<

head :: (HSStream p a) -> a
head (a :< _) = a

tail :: HSStream p a -> HSStream p a
tail (_ :< s) = s

repeat :: a -> HSStream p a
repeat a = a :< repeat a

-- | Map allows phantom change...
--
map :: (a -> b) -> HSStream p a -> HSStream q b
map f (a :< s) = f a :< map f s 

zip :: (a -> b -> c) -> HSStream p a -> HSStream p b -> HSStream p c
zip f sx sy = step sx sy 
  where
    step (a :< sa) (b :< sb) = f a b :< step sa sb  





instance Num a => Num (HSStream p a) where
  (+)           = zip (+)
  (-)           = zip (-) 
  (*)           = zip (*)
  abs           = map abs
  negate        = map negate
  signum        = map signum
  fromInteger i = repeat (fromInteger i)

iterate :: (a -> a) -> a -> HSStream p a
iterate f a = a :< iterate f (f a)

infixr 5 <<

(<<) :: [a] -> HSStream p a -> HSStream p a
[]     << s = s
(x:xs) << s = x :< (xs << s)

diff :: Num a => HSStream p a -> HSStream p a
diff s = tail s - s

sum :: Num a => HSStream p a -> HSStream p a
sum s = t where t = 0 :< t + s


const :: Num a => a -> HSStream p a
const n = n :< repeat 0


(*>) :: Num a => a -> HSStream p a -> HSStream p a
(*>) x s = map (x*) s

merge :: HSStream p a -> HSStream p a -> HSStream p a
merge (a :< sa) (b :< sb) = a :< b :< merge sa sb

cycle :: [a] -> HSStream p a
cycle xs = foldr (:<) (cycle xs) xs


take :: Int -> HSStream p a -> [a]
take n _          | n <= 0 = []
take n (a :< sa)           = a : take (n-1) sa


prefix :: Int -> HSStream p a -> HSStream p a -> HSStream p a
prefix n s t = go n s
  where
    go i _ | i <= 0 = t
    go i (x :< xs)  = x :< go (i-1) xs


drop :: Int -> HSStream p a -> HSStream p a
drop n s          | n <= 0 = s
drop n (_ :< sa)           = drop (n-1) sa



(>*<) :: Num a => HSStream p a -> HSStream p a -> HSStream p a
(>*<) (b0 :< bq) a@(a0 :< aq) = b0 * a0 :< aq + bq >*< a


(>/<) :: Fractional a => HSStream p a -> HSStream p a -> HSStream p a
(>/<) (b0 :< bq) a@(a0 :< aq) = w0 :< (bq - w0 *> aq) >/< a
  where
    w0 = b0 / a0



-- | 'duplicate' is a stream transformer so it allows phantom 
-- change.
--
duplicate :: Int -> HSStream p1 a -> HSStream p2 a
duplicate count (s :< ss) = s :< go 1 s ss
  where
    go n t   ts        | n < count  = t :< go (n+1) t ts
    go _ _   (t :< ts) | otherwise  = t :< go 1     t ts


skip :: Int -> HSStream p1 a -> HSStream p2 a
skip count (s :< ss) = s :< go 1 ss
  where
    go n (_ :< ts) | n < count  = go (n+1) ts
    go _ (t :< ts) | otherwise  = t :< go 1 ts

