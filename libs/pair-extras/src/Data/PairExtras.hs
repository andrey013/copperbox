{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PairExtras
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- More functions on pairs... c.f. Data.Tuple.
-- @fork@ and @prod@ are the useful ones, the others are 
-- somewhat synthetic.
-- 
-----------------------------------------------------------------------------

module Data.PairExtras
  ( 
    
    fork
  , prod
  , dup
  , swap

  , cprod  
  , outer
  , inner
  , firsts
  , seconds

  , undistl
  , distl

  ) where


-- | Apply the functions @f@ and @g@ to the element @a@, 
-- returning the resulting pair.
fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) a = (f a,g a)

-- | The /product/ function. 
-- Apply the function @f@ to the first element of the pair,
-- and apply the function @g@ to the second element.
prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

-- exl and exr are fst and snd respectively so are not defined here.

-- | dup aka /Duplicate/.
-- Duplicate the supplied value returning a pair. This is equivalent 
-- to @fork (id,id) a@.
dup :: a -> (a,a)
dup a = (a,a)

-- | Swap the elements of the pair.
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- | Curried version of 'prod'.
cprod :: (a -> c) -> (b -> d) -> a -> b -> (c,d)
cprod f g = curry $ prod f g

-- | Return the /outer/ elements of the argument pairs.
outer :: (a,b) -> (c,d) -> (a,d)
outer = curry $ prod fst snd

-- | Return the /inner/ elements of the argument pairs.
inner :: (a,b) -> (c,d) -> (b,c)
inner = cprod snd fst

-- | Return the /first/ elements of the argument pairs.
firsts :: (a,b) -> (c,d) -> (a,c)
firsts = cprod fst fst

-- | Return the /second/ elements of the argument pairs.
seconds :: (a,b) -> (c,d) -> (b,d)
seconds = cprod snd snd

-- | @undistl@.
undistl :: Either (a,b) (a,c) -> (a, Either b c) 
undistl (Left (a,b))  = (a,Left b)
undistl (Right (a,c)) = (a,Right c)

-- | @distl@.
distl :: (a, Either b c) -> Either (a,b) (a,c)
distl (a,Left b)  = Left (a,b)
distl (a,Right c) = Right (a,c)

