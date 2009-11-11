{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.PairCalculus
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Pair calculus presented by Jeremy Gibbons in ...
-- 'Calculating Functional Programs'
--
-----------------------------------------------------------------------------

module Data.Aviary.PairCalculus
  ( 

    
  -- * Fork, exl, exr ...
    fork
  , exl
  , exr
  , pmap
  , undistl
  , distl

  ) where



--------------------------------------------------------------------------------


-- | Apply the functions @f@ and @g@ to the element @a@, 
-- returning the resulting pair. 
fork :: (a -> b,a -> c) -> a -> (b,c)
fork (f,g) a = (f a,g a)


-- | Destructor extracting the left component - aka fst.
exl :: (a,b) -> a
exl (a,_b) = a

-- | Destructor extracting the left component - aka fst.
exr :: (a,b) -> b
exr (_a,b) = b

-- | The product bifunctor.
pmap :: (a -> c,b -> d) -> (a,b) -> (c,d)
pmap (f,g) (a,b) = (f a,g b)



-- | @undistl@.
undistl :: Either (a,b) (a,c) -> (a, Either b c) 
undistl (Left (a,b))  = (a,Left b)
undistl (Right (a,c)) = (a,Right c)

-- | @distl@.
distl :: (a, Either b c) -> Either (a,b) (a,c)
distl (a,Left b)  = Left (a,b)
distl (a,Right c) = Right (a,c)



{-

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


-- | A uncurried version of fork - i.e. the functions @f@ and @g@ 
-- are supplied as a pair, Landin-style in the terminology of 
-- Davie. This is the definition of /fork/ given by Gibbons. 
ufork :: (a -> b, a -> c) -> a -> (b,c) 
ufork (f,g) a = (f a, g a)  

-- ufork = uncurry fork


-- | Variant of 'prod' where the supplied is data @(a,b)@ is 
-- extracted from its pair into curried form @... a -> b ...@.
prodc :: (a -> c) -> (b -> d) -> a -> b -> (c,d)
prodc f g = curry $ prod f g


-- | @undistl@.
undistl :: Either (a,b) (a,c) -> (a, Either b c) 
undistl (Left (a,b))  = (a,Left b)
undistl (Right (a,c)) = (a,Right c)




-- Projections on two pairs

-- | Return the /outer/ elements of the argument pairs.
outer :: (a,b) -> (c,d) -> (a,d)
outer = curry $ prod fst snd

-- | Return the /inner/ elements of the argument pairs.
inner :: (a,b) -> (c,d) -> (b,c)
inner = prodc snd fst

-- | Return the /first/ elements of the argument pairs.
firsts :: (a,b) -> (c,d) -> (a,c)
firsts = prodc fst fst

-- | Return the /second/ elements of the argument pairs.
seconds :: (a,b) -> (c,d) -> (b,d)
seconds = prodc snd snd

-}
