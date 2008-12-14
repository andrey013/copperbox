{-# OPTIONS -Wall #-}

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
import Prelude hiding ( null, span, replicate, zipWith )

number :: Int -> Seq a -> Seq (Int,a) 
number start se = sziplWith (flip (,)) se [start..]

zip :: Seq a -> Seq b -> Seq (a,b)
zip = zipWith (,)

szipl :: Seq a -> [b] -> Seq (a,b)
szipl = sziplWith (,)


-- The result will be as long as the shortest seq, 
-- this is the behaviour of list zip. 
zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f sx sy = step (viewl sx) (viewl sy) where
    step (a :< sa) (b :< sb)  = (f a b) <| step (viewl sa) (viewl sb)
    step _    _               = empty

sziplWith :: (a -> b -> c) -> Seq a -> [b] -> Seq c
sziplWith f se ss = step (viewl se) ss where
  step (a :< sa) (x:xs)   = f a x <| step (viewl sa) xs
  step _         _        = empty

lzipsWith :: (a -> b -> c) -> [a] -> Seq b -> [c]
lzipsWith f ss se = step ss (viewl se) where
  step (x:xs) (y :< sy)   = f x y : step xs (viewl sy)
  step _      _           = []
    
replicate :: Int -> a -> Seq a
replicate i a | i <= 0     = empty
              | otherwise  = a <| replicate (i-1) a 
  
smaximum :: (Ord a) => (Seq a) -> a
smaximum se | null se = error "smaximum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr max a sa 

sminimum :: (Ord a) => (Seq a) -> a
sminimum se | null se = error "sminimum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr min a sa

concat :: Seq (Seq a) -> Seq a
concat = F.foldr (><) empty

genConcat :: F.Foldable c => 
    (z a -> z a -> z a) -> (z a) -> (t a -> z a) -> c (t a) -> z a
genConcat cat empt conv = F.foldr fn empt where
  fn a b = (conv a) `cat` b

-- The inner collection type is preserved so just use F.foldr
concat_ls's :: [Seq a] -> Seq a
concat_ls's = F.foldr (><) empty

-- The inner collection type is changed so use genConcat
concat_ls'l :: [Seq a] -> [a]
concat_ls'l = genConcat (++) [] (F.toList)


transpose :: Seq (Seq a) -> Seq (Seq a)
transpose = step . viewl
  where
    step EmptyL         = empty
    step (x :< sse)     = case viewl x of
        EmptyL    -> transpose sse
        (e :< se) -> (e <| allheads sse) <| (transpose $ se <| alltails sse)
    
    allheads = F.foldl' (\acc se -> acc |> head1 se) empty
    alltails = F.foldl' (\acc se -> acc |> tail1 se) empty
    
    head1 se = case viewl se of EmptyL -> unmatchErr; 
                                (a :< _) -> a
    
    tail1 se = case viewl se of EmptyL -> unmatchErr; 
                                (_ :< sa) -> sa
                                
    unmatchErr = error "stranspose - sequences of unequal length" 
    
filter :: (a -> Bool) -> Seq a -> Seq a
filter pf = step . viewl where
  step EmptyL     = empty
  step (a :< sa) | pf a       = a <| step (viewl sa)
                 | otherwise  = step (viewl sa)

groupBy                 :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy eq se           = step (viewl se) where
    step EmptyL         = empty
    step (a :< sa)      = (a <| sy) <| step (viewl sz) where
                            (sy,sz) = span (eq a) sa


span                    :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
span p se               = step (viewl se) where
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
              


        
