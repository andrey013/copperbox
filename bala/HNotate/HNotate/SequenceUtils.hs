
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.SequenceUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- Extra functions for Data.Sequence
--
--------------------------------------------------------------------------------

module HNotate.SequenceUtils where

-- Should have no dependencies on other HNotate modules

import qualified Data.Foldable as F
import Data.Sequence
import Prelude hiding (null)

number :: Int -> Seq a -> Seq (Int,a) 
number start se = sziplWith (flip (,)) se [start..]

szip :: Seq a -> Seq b -> Seq (a,b)
szip = szipWith (,)

szipl :: Seq a -> [b] -> Seq (a,b)
szipl = sziplWith (,)


-- The result will be as long as the shortest seq, 
-- this is the behaviour of list zip. 
szipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
szipWith f sa sb = step (viewl sa) (viewl sb) where
    step (a :< sa) (b :< sb)  = (f a b) <| step (viewl sa) (viewl sb)
    step _    _               = empty

sziplWith :: (a -> b -> c) -> Seq a -> [b] -> Seq c
sziplWith f se xs = step (viewl se) xs where
  step (a :< sa) (x:xs)   = f a x <| step (viewl sa) xs
  step _         _        = empty

lzipsWith :: (a -> b -> c) -> [a] -> Seq b -> [c]
lzipsWith f xs se = step xs (viewl se) where
  step (x:xs) (y :< sy)   = f x y : step xs (viewl sy)
  step _      _           = []
    
sreplicate :: Int -> a -> Seq a
sreplicate i a | i <= 0     = empty
               | otherwise  = a <| sreplicate (i-1) a 
  
smaximum :: (Ord a) => (Seq a) -> a
smaximum se | null se = error "smaximum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr max a se 

sminimum :: (Ord a) => (Seq a) -> a
sminimum se | null se = error "sminimum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr min a se

sconcat :: Seq (Seq a) -> Seq a
sconcat = F.foldr (><) empty

stranspose :: Seq (Seq a) -> Seq (Seq a)
stranspose = step . viewl
  where
    step EmptyL         = empty
    step (x :< sse)     = case viewl x of
        EmptyL    -> stranspose sse
        (e :< se) -> (e <| allheads sse) <| (stranspose $ se <| alltails sse)
    
    allheads = F.foldl' (\acc se -> acc |> head1 se) empty
    alltails = F.foldl' (\acc se -> acc |> tail1 se) empty
    
    head1 se = case viewl se of EmptyL -> unmatchErr; 
                                (a :< _) -> a
    
    tail1 se = case viewl se of EmptyL -> unmatchErr; 
                                (_ :< sa) -> sa
                                
    unmatchErr = error "stranspose - sequences of unequal length" 
    
sfilter :: (a -> Bool) -> Seq a -> Seq a
sfilter pf = step . viewl where
  step EmptyL     = empty
  step (a :< sa) | pf a       = a <| step (viewl sa)
                 | otherwise  = step (viewl sa)

sgroupBy                :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
sgroupBy eq se          = step (viewl se) where
    step EmptyL         = empty
    step (a :< sa)      = (a <| sy) <| step (viewl sz) where
                            (sy,sz) = sspan (eq a) sa


sspan                   :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
sspan p se              = step (viewl se) where
    step EmptyL         = (empty,empty)
    step (a :< sa)      
            | p a       = let (sy,sz) = step (viewl sa) in (a <| sy, sz)
            | otherwise = (empty, a <| sa)                             

unseqMap :: (a -> b) -> Seq a -> [b]
unseqMap f = F.foldr (\e a -> (f e) : a) []     



--------------------------------------------------------------------------------
-- Recursion schemes

-- Catamorphism - foldr
cata :: (a -> b -> b) -> b -> Seq a -> b
cata phi b = step . viewl 
  where step EmptyL  = b
        step (e:<se) = phi e (step (viewl se))


-- anamorphism - unfoldr
ana :: (b -> Maybe (a,b)) -> b -> Seq a
ana phi = step
  where step b = case phi b of
                  Nothing -> empty
                  Just (a,s) -> a <| step s

-- hylomorphism
hylo :: (b -> Maybe (a, b)) -> (a -> c -> c) -> c -> b -> c
hylo f g a = step 
  where step b = case f b of
                   Nothing -> a
                   Just (x,s) -> g x (step s)

-- paramorphism (generalizes cata)
para :: (a -> (Seq a, b) -> b) -> b -> Seq a -> b
para phi b = step . viewl
  where step EmptyL  = b
        step (e:<se) = phi e (se, step (viewl se))

-- apomorphism (generalizes ana)
apo :: (b -> Maybe (a, b)) -> (b -> Seq a) -> b -> Seq a
apo phi chi b = case phi b of
              Just (a, b') -> a <| apo phi chi b'
              Nothing -> chi b


-- order of args is (cata)para
zygo :: (a -> b -> b) -> (a -> (Seq a, b) -> b) -> b -> Seq a -> b
zygo f g b = step . viewl 
  where step EmptyL  = b
        step (e:<se) = f e (g e (se, (step (viewl se))))


--------------------------------------------------------------------------------
-- Monadic versions

cataM :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
cataM phi b = mf . viewl
  where
    mf EmptyL   = return b
    mf (e:<se)  = do y <- mf (viewl se)
                     phi e y


anaM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (Seq a)
anaM g b = g b >>= maybe (return empty)
                         (\(x,b') -> anaM g b' >>= return . (x <|)) 

-- hylomorphism
hyloM :: Monad m => (a -> c -> m c) -> (b -> m (Maybe (a, b))) -> c -> b -> m c
hyloM d f a = step 
  where step b = f b >>= maybe (return a) (\(x,s) -> step s >>= (d x))

-- paramorphism 
paraM :: Monad m => (a -> (Seq a, b) -> m b) -> b -> Seq a -> m b
paraM phi b = step . viewl
  where step EmptyL  = return b
        step (e:<se) = do se' <- step (viewl se) 
                          phi e (se, se')

-- apomorphism 
apoM :: Monad m => (b -> m (Maybe (a, b))) -> (b -> m (Seq a)) -> b -> m (Seq a)
apoM phi chi b = 
  phi b >>= maybe (chi b)
                  (\(a, b') -> apoM phi chi b' >>= return . (a <|))
              


        
