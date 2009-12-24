{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.SequenceRecursion
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- (Some of) the famous recursion schemes for Data.Sequence. 
--
--------------------------------------------------------------------------------

module GreyFold.Base.SequenceRecursion where


import Data.Sequence




-- | Catamorphism - foldr.
cata :: (a -> b -> b) -> b -> Seq a -> b
cata f b se = step (viewl se) where 
    step EmptyL     = b
    step (a :< sa)  = f a (step (viewl sa))


-- | Anamorphism - unfoldr.
ana :: (b -> Maybe (a,b)) -> b -> Seq a
ana f b0 = step (f b0) where
    step Nothing        = empty
    step (Just (a,st))  = a <| step (f st)

-- | Hylomorphism.
-- A hylomorphism has no dependency of Data.Sequence of course.
hylo :: (a -> c -> c) -> (b -> Maybe (a, b)) -> c -> b -> c
hylo f g c0 b0 = step (g b0) where
    step Nothing        = c0
    step (Just (a,st))  = f a (step (g st))

-- | Paramorphism (generalizes cata).
para :: (a -> (Seq a, b) -> b) -> b -> Seq a -> b
para f b0 se = step (viewl se) where
    step EmptyL     = b0
    step (a :< sa)  = f a (sa, step (viewl sa))

-- | Apomorphism (generalizes ana).
apo :: (b -> Maybe (a, b)) -> (b -> Seq a) -> b -> Seq a
apo f g b0 = step (f b0) where
    step Nothing        = g b0
    step (Just (a,st))  = a <| step (f st)



-- | Zygomorphism.
zygo :: (a -> b -> b) -> (a -> (Seq a, b) -> b) -> b -> Seq a -> b
zygo f g b se = step (viewl se) where
    step EmptyL     = b
    step (a :< sa)  = f a (g a (sa, (step (viewl sa))))


--------------------------------------------------------------------------------
-- Monadic versions

-- | Monadic catamorphism.
cataM :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
cataM f b se = step (viewl se) where
    step EmptyL     = return b
    step (a :< sa)  = do x <- step (viewl sa)
                         f a x


-- | Monadic anamorphism.
anaM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (Seq a)
anaM f b0 = f b0 >>= step where
    step Nothing        = return empty
    step (Just (a,st))  = do x <- f st >>= step
                             return (a <| x) 
    
-- | Monadic hylomorphism.
hyloM :: Monad m => (a -> c -> m c) -> (b -> m (Maybe (a, b))) -> c -> b -> m c
hyloM f g c0 b0 = g b0 >>= step where
    step Nothing        = return c0 
    step (Just (a,st))  = do x <- g st >>= step
                             f a x

-- | Monadic paramorphism. 
paraM :: Monad m => (a -> (Seq a, b) -> m b) -> b -> Seq a -> m b
paraM f b0 se = step (viewl se) where
    step EmptyL     = return b0
    step (a :< sa)  = do st <- step (viewl sa) 
                         f a (se,st)


-- | Monadic apomorphism. 
apoM :: Monad m => (b -> m (Maybe (a, b))) -> (b -> m (Seq a)) -> b -> m (Seq a)
apoM f g b0 = f b0 >>= step where
    step Nothing        = g b0
    step (Just (a,st))  = do sa <- f st >>= step
                             return (a <| sa)

 
-- | Monadic zygomorphism. 
zygoM :: Monad m => 
         (a -> b -> m b) -> (a -> (Seq a, b) -> m b) -> b -> Seq a -> m b
zygoM f g b se = step (viewl se) where 
    step EmptyL       = return b
    step (a :< sa)    = do st <- step (viewl sa)
                           x  <- g a (sa,st)
                           f a x
        
